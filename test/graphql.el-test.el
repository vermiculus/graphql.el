;;; graphql.el-test.el --- Tests for graphql.el

(ert-deftest encode-basic ()
  (should (string= (graphql-encode
                    '(query
                      hello-world))
                   "query { hello-world }"))

  (should (string= (graphql-encode
                    '(query
                      :arguments ((one . "1")
                                  (two . "2"))
                      hello-world))
                   "query(one:\"1\",two:\"2\") { hello-world }"))

  (should (string= (graphql-encode
                    '(query
                      :arguments ((one . ($ variableForOne))
                                  (two . "2"))
                      hello-world))
                   "query(one:$variableForOne,two:\"2\") { hello-world }")))

(ert-deftest encode-recursive ()
  (should (string= (graphql-encode
                    '(query
                      (repository
                       :arguments ((owner . "my-owner")
                                   (name . "my-repo-name")))))
                   "query { repository(owner:\"my-owner\",name:\"my-repo-name\") }"))

  (should (string= (graphql-encode
                    '(query
                      (repository
                       :arguments ((owner . "my-owner")
                                   (name . "my-repo-name"))
                       (issues
                        :arguments ((first . 20))
                        (edges (node number title url))))))
                   (concat
                    "query { repository(owner:\"my-owner\",name:\"my-repo-name\") "
                    "{ issues(first:20) { edges { node { number title url } } } } }")))

  (should (string= (graphql-encode
                    '(addReaction :arguments ((input . ((subjectId . "MDU6SXNzdWUxNzc2MzA3Mjk=")
                                                        (content . HOORAY))))))
                   "addReaction(input:{subjectId:\"MDU6SXNzdWUxNzc2MzA3Mjk=\",content:HOORAY})")))

;;; graphql.el-test.el ends here
