;;; graphql-test.el --- Tests for graphql.el

(require 'graphql)

(ert-deftest encode-basic ()
  (should (string= (graphql-encode
                    '(query
                      hello-world))
                   "query{hello-world}"))

  (should (string= (graphql-encode
                    '(query
                      :arguments ((one . 1)
                                  (two . "2"))
                      hello-world))
                   "query(one:1,two:\"2\"){hello-world}"))

  (should (string= (graphql-encode
                    '(query
                      :arguments ((one . ($ variableForOne))
                                  (two . "2"))
                      hello-world))
                   "query(one:$variableForOne,two:\"2\"){hello-world}")))

(ert-deftest encode-recursive ()
  (should (string= (graphql-encode
                    '(query
                      (repository
                       :arguments ((owner . "my-owner")
                                   (name . "my-repo-name")))))
                   "query{repository(owner:\"my-owner\",name:\"my-repo-name\")}"))

  (should (string= (graphql-encode
                    '(query
                      (repository
                       :arguments ((owner . "my-owner")
                                   (name . "my-repo-name"))
                       (issues
                        :arguments ((first . 20))
                        (edges (node number title url))))))
                   "query{repository(owner:\"my-owner\",name:\"my-repo-name\"){issues(first:20){edges{node{number title url}}}}}"))

  (should (string= (graphql-encode
                    '(addReaction :arguments ((input . ((subjectId . "MDU6SXNzdWUxNzc2MzA3Mjk=")
                                                        (content . HOORAY))))))
                   "addReaction(input:{subjectId:\"MDU6SXNzdWUxNzc2MzA3Mjk=\",content:HOORAY})")))

(ert-deftest encode-query ()
  (should (string= (graphql-query (repository))
                   "query{repository}"))
  (should (string= (graphql-query (test)
                                  (repository))
                   "query test{repository}"))
  (should (string= (graphql-query (test ((ep Episode !)
                                         (review ReviewInput ! . 50)))
                                  (repository :arguments ((hello . ($ ep)))))
                   "query test($ep:Episode!,$review:ReviewInput!=50){repository(hello:$ep)}"))
  (should (string= (graphql-mutation (testMutation ((ep Episode . ((complex . params)))))
                                     (change thing))
                   "mutation testMutation($ep:Episode!){change{thing}}")))

(ert-deftest encode-complicated ()
  (should (string= (graphql-query (test ((ep Episode . ((complex . params)
                                                        (with . "values")
                                                        (like . 50)))
                                         (review ReviewInput ! . 50)))
                                  (repository :arguments ((hello . ($ ep)))))
                   "query test($ep:Episode={complex:params,with:\"values\",like:50},$review:ReviewInput!=50){repository(hello:$ep)}")))
;;; graphql-test.el ends here
