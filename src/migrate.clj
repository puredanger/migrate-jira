(ns migrate
  (:require
    [clojure.java.io :as jio]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.pprint :as pprint]
    [clojure.data.json :as json])
  (:import
    [java.io InputStream FileInputStream BufferedInputStream]
    [javax.xml.stream XMLInputFactory XMLStreamConstants XMLStreamReader]))

(set! *warn-on-reflection* true)

(def codes
  {XMLStreamConstants/START_ELEMENT :start-elem
   XMLStreamConstants/ATTRIBUTE :attr
   XMLStreamConstants/NAMESPACE :ns
   XMLStreamConstants/END_ELEMENT :end-elem
   XMLStreamConstants/CHARACTERS :char
   XMLStreamConstants/CDATA :cdata
   XMLStreamConstants/COMMENT :comment
   XMLStreamConstants/SPACE :space
   XMLStreamConstants/START_DOCUMENT :start-doc
   XMLStreamConstants/END_DOCUMENT :end-doc})

(defn read-attrs
  [^XMLStreamReader rdr]
  (let [c (.getAttributeCount rdr)]
    (apply conj {}
      (for [i (range c)] [(.getAttributeLocalName rdr i) (.getAttributeValue rdr i)]))))


(defn load-xml
  [^String f]
  (with-open [s (BufferedInputStream. (FileInputStream. f))]
    (let [factory (doto ^XMLInputFactory (XMLInputFactory/newInstance)
                    (.setProperty "javax.xml.stream.isCoalescing" true))
          ^XMLStreamReader r (.createXMLStreamReader ^XMLInputFactory factory s)]
      (.nextTag r) ;; skip to container
      (loop [data {}, last-entity nil, last-sub nil]
        (if (.hasNext r)
          (case (get codes (.next r) :other)
            :start-elem
            (do
              ;(println "start-elem " (.. r getName toString))
              (if last-entity
                (recur data last-entity (.. r getName toString)) ;; new sub attr
                (recur data (assoc (read-attrs r) :tag (.. r getName toString)) nil))) ;; new entity

            :end-elem
            (if last-entity
              (if last-sub
                (recur data last-entity nil) ;; pop sub-entity
                (recur (update-in data [(:tag last-entity)] conj last-entity) nil nil)) ;; pop and add entity
              (recur data nil nil)) ;; container

            :char
            (if (and last-entity last-sub)
              (recur data (assoc last-entity last-sub (.getText r)) last-sub) ;; sub chars
              (recur data last-entity last-sub)) ;; unknown chars

            (recur data last-entity last-sub)) ;; all other cases, ignore
          data)))))

(defn transform-date
  [date]
  (str (subs date 0 10) "T" (subs date 11) "-0600"))

(defn transform-versions
  [data project-id]
  (for [version (->> (get data "Version") (filter #(= project-id (get % "project"))))]
    (cond-> {"name" (get version "name")}
      (= "true" (get version "released"))
      (assoc "released" "true")
      (get version "releasedate")
      (assoc "releasedate" (transform-date (get version "releasedate"))))))

(defn transform-components
  [data project-id]
  (->> (get data "Component")
    (filter #(= project-id (get % "project")))
    (map #(get % "name"))))

(def priority-codes
  {"1" "Blocker"
   "2" "Critical"
   "3" "Major"
   "4" "Minor"
   "5" "Trivial"})

(def status-codes
  {"1" "Open"
   "3" "In Progress"
   "4" "Reopened"
   "5" "Resolved"
   "6" "Closed"})

(def issue-type-codes
  {"1" "Bug"
   "3" "Task"
   "4" "Improvement"
   "5" "New Feature"})

(def resolution-codes
  {"1" "Completed"
   "2" "Declined"
   "3" "Duplicate"
   "4" "Not Reproducible"})

(defn find-labels
  [data issue-id]
  (->> (get data "Label")
    (filter #(= issue-id (get % "issue")))
    (map #(get % "label"))
    (remove #{"enhancement" "bug" "test" "patch"})
    seq))

(defn linked-versions
  [data issue-id]
  (let [versions (group-by #(get % "id") (get data "Version"))]
    (->> (get data "NodeAssociation")
      (filter #(let [{:strs [sourceNodeId associationType]} %]
                 (and (= associationType "IssueVersion") (= sourceNodeId issue-id))))
      (map #(get % "sinkNodeId"))
      (map #(get (first (get versions %)) "name"))
      vec)))

(defn fixed-versions
  [data issue-id]
  (let [versions (group-by #(get % "id") (get data "Version"))]
    (->> (get data "NodeAssociation")
      (filter #(let [{:strs [sourceNodeId associationType]} %]
                 (and (= associationType "IssueFixVersion") (= sourceNodeId issue-id))))
      (map #(get % "sinkNodeId"))
      (map #(get (first (get versions %)) "name"))
      vec)))

(defn issue-components
  [data issue-id]
  (let [versions (group-by #(get % "id") (get data "Component"))]
    (->> (get data "NodeAssociation")
      (filter #(let [{:strs [sourceNodeId associationType]} %]
                 (and (= associationType "IssueComponent") (= sourceNodeId issue-id))))
      (map #(get % "sinkNodeId"))
      (map #(get (first (get versions %)) "name"))
      vec)))

(defn attachments
  [data active pkey ikey issue-id]
  (->> (get data "FileAttachment")
    (filter #(= issue-id (get % "issue")))
    (map #(let [{:strs [id issue filename created author]} %]
            (hash-map
              "name" filename
              "attacher" author
              "created" (transform-date created)
              "uri" (str "http://cdn.cognitect.com/jira/attachments/" pkey "/" ikey "/" id)
              ;;"description" nil
              )))
    vec))

(defn custom-fields
  [data issue-id]
  (->> (get data "CustomFieldValue")
    (filter #(= issue-id (get % "issue")))
    (map #(let [{:strs [customfield stringvalue]} %]
            (case customfield
              "10000" {"fieldName" "Patch"
                       "fieldType" "com.atlassian.jira.plugin.system.customfieldtypes:select"
                       "value" (get {"10001" "Code", "10002" "Code and Test"} stringvalue)}
              "10002" {"fieldName" "Approval"
                       "fieldType" "com.atlassian.jira.plugin.system.customfieldtypes:select"
                       "value" (get {"10120" "Triaged", "10220" "Prescreened",
                                     "10003" "Vetted", "10004" "Screened",
                                     "10005" "Accepted", "10006" "Incomplete",
                                     "10007" "Ok"} stringvalue)}
              nil)))
    (remove nil?)
    vec))

(defn comments
  [data active issue-id]
  (->> (get data "Action")
    (filter #(= issue-id (get % "issue")))
    (map #(let [{:strs [author created body]} %]
            {"body" (if (contains? active author) body (str "_Comment made by: " author "_\n\n" body))
             "author" author
             "created" (transform-date created)}))
    vec))

(defn history-items
  [data group-id]
  (->> (get data "ChangeItem")
    (filter #(= group-id (get % "group")))
    (map #(let [{:strs [fieldtype field newvalue newstring oldvalue oldstring]} %]
            (if (contains? #{"Waiting On" "Workflow"} field)
              nil
              (cond->
                {"fieldType" fieldtype, "field" field}
                oldstring (assoc "fromString" oldstring)
                newstring (assoc "toString" newstring)
                ;; ("assignee" "reporter" "Comment" "description" "summary"
                ;                "Key" "environment" "labels")
                ;;oldvalue (assoc "from" oldvalue)
                ;;newvalue (assoc "to" newvalue)
                ))))
    (remove nil?)
    vec))

(defn history
  [data active issue-id]
  (->> (get data "ChangeGroup")
    (filter #(= issue-id (get % "issue")))
    (map #(let [{:strs [id author created]} %]
            {"author" author
             "created" (transform-date created)
             "items" (history-items data id)}))))

(defn voters
  [data active issue-id]
  (->> (get data "UserAssociation")
    (filter #(and (= "VoteIssue" (get % "associationType")) (= issue-id (get % "sinkNodeId"))))
    (map #(get % "sourceName"))
    (filter #(contains? active %))
    distinct
    vec))

(defn watchers
  [data active issue-id]
  (->> (get data "UserAssociation")
    (filter #(and (= "WatchIssue" (get % "associationType")) (= issue-id (get % "sinkNodeId"))))
    (map #(get % "sourceName"))
    (filter #(contains? active %))
    distinct
    vec))

(defn transform-issues
  [data project-id pkey active]
  (->> (get data "Issue")
    (filter #(= project-id (get % "project")))
    (map #(let [{:strs [id key priority description status reporter type
                        resolution created updated summary assignee environment]} %
                affectedVs (linked-versions data id)
                fixVs (fixed-versions data id)
                labels (find-labels data id)
                components (issue-components data id)
                customfields (custom-fields data id)
                comments (comments data active id)
                attachments (attachments data active pkey key id)
                voters (voters data active id)
                watchers (watchers data active id)
                history (history data active id)]
            (cond->
              (hash-map
                ;;"externalId" nil
                "key" key
                "priority" (priority-codes priority)
                "status" (status-codes status)
                "reporter" reporter
                "issueType" (issue-type-codes type)
                "created" (transform-date created)
                "updated" (transform-date updated)
                "summary" summary)
              description (assoc "description" description)
              environment (assoc "environment" environment)
              (seq labels) (assoc "labels" labels)
              resolution (assoc "resolution" (resolution-codes resolution))
              assignee (assoc "assignee" assignee)
              (seq affectedVs) (assoc "affectedVersions" affectedVs)
              (seq fixVs) (assoc "fixedVersions" fixVs)
              (seq components) (assoc "components" components)
              (seq customfields) (assoc "customFieldValues" customfields)
              (seq comments) (assoc "comments" comments)
              (seq attachments) (assoc "attachments" attachments)
              (seq watchers) (assoc "watchers" watchers)
              (seq voters) (assoc "voters" voters)
              (seq history) (assoc "history" history))))))

;; user data is bad, must exclude
(def excludes #{"monorok","gar3thjon3s"})

(defn active-users
  "Created or commented in last 4 years + anyone that has edited a ticket."
  [data]
  (let [;; created
        ;creators (->> (get data "Issue")
        ;           (filter #(contains? #{"2019" "2018" "2017" "2016" "2015"}
        ;                      (subs (get % "created") 0 4)))
        ;           (map #(get % "reporter"))
        ;           set)
        ;;; commented
        ;commenters (->> (get data "Action")
        ;             (filter #(contains? #{"2019" ;; "2018" "2017" "2016" "2015"
        ;                                   }
        ;                        (subs (get % "updated") 0 4)))
        ;             (map #(get % "updateauthor"))
        ;             set)
        ;;; edited
        editors (->> (get data "ChangeGroup")
                  (map #(get % "author"))
                  (remove #(excludes %))
                  set)]
    editors))

(defn export-users
  [data active]
  (println "Active users" (count active))
  (let [all-users (get data "User")
        _ (println "All users" (count all-users))
        users (for [name active]
                (when-let [{:strs [userName emailAddress displayName]}
                           (->> all-users
                             (filter #(= name (get % "userName")))
                             (sort-by #(get % "updatedDate"))
                             last)]
                  {"name" userName
                   "groups" ["jira-users" "jira-developers"]
                   "active" true
                   "email" emailAddress
                   "fullname" displayName}))
        users (remove nil? users)
        _ (println "Imported users" (count users))
        j (with-out-str (json/pprint {"users" users}))]
    (spit "users.json" j)))

(defn export-project
  [data active key]
  (let [{:strs [id name description]} (->> (get data "Project") (filter #(= key (get % "key"))) first)
        _ (println "Exporting" key)
        versions (transform-versions data id)
        components (transform-components data id)
        issues (transform-issues data id key active)
        p {"projects"
           [(cond-> {"name" name
                     "key" key
                     "type" "software"}
              description (assoc "description" description)
              (seq versions) (assoc "versions" versions)
              (seq components) (assoc "components" components)
              (seq issues) (assoc "issues" issues))]}
        j (with-out-str (json/pprint p))]
    (spit (str "project-" key ".json") j)))

(defn export-projects
  [data active]
  (doseq [id (->> (get data "Project") (map #(get % "key")))]
    (export-project data active id)))

(defn export-all
  [f]
  (let [data (load-xml f)
        active (active-users data)]
    (export-users data active)
    (export-projects data active)))

(comment
  (def xs (load-xml "../jirabackup20190516/entities.xml"))
  (export-users xs (active-users xs))

  (filter #(= "alexmiller" (get % "userName")) (get xs "User"))

  (export-project xs (active-users xs) "CCACHE")
  (first (get xs "Project"))
  (sort (keys xs))
  (first (get xs "Issue"))

  (def clj (doall (transform-issues xs "10010" "CLJ" (active-users xs))))
  (take 20 (filter #(get % "history") clj))

  (take 10 (get xs "ChangeItem"))

  (->> (get xs "UserAssociation") (map #(get % "associationType")) distinct sort)

  (get xs "Component")
  (transform-versions xs "10010")
  (transform-components xs "10271")
  (def iss (transform-issues xs "10010"))
  (->> (get xs "Issue") vals (keep #(get % "status")) frequencies)
  (count iss)
  (take 10 iss)

  (distinct (map #(get % "associationType") (get xs "UserAssociation")))

  (first (get xs "User"))
  (map #(get % "name")) (sort (active-users xs))

  (count (active-users xs))
  (take 10 (vals (get xs "ChangeGroup")))

  (let [project-id "10010"
        data xs]
    (for [version (->> (get data "Version") (filter #(= project-id (get % "project"))))]
      (cond-> {"name" (get version "name")}
        (= "true" (get version "released"))
        (assoc "released" "true" "releasedate" (transform-date (get version "releasedate"))))))

  (first (get xs "Version"))
  (transform-versions xs "10040")

  (->> (get xs "NodeAssociation") (map #(get % "associationType")) distinct sort)

  (export-all "../jirabackup20190516/entities.xml")

  )

