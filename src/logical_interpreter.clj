(ns logical-interpreter)

(use 'clojure.string)


(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]

  ; Por cada línea:
  ;   Si es un fact y coinciden los párámetros, devolver true
  ;   Si es una regla y coincide el nombre, evaluar las condiciones
  
  (let [
        predicates (map trim (split-lines database))
        [_ function-name parameters] (re-matches #"(.*?)\((.*?)\)" query)
        ]
      
        (printf "Searching for rule '%s' parameters: '%s'\n" function-name parameters)

        ; Buscamos algún fact que matchee la query
        
        (if (filter 
                  #(re-find
                    (re-pattern (format "^%s\\(%s\\)\\." function-name parameters))
                    %)
            predicates)
          true
          false)
        
        
        )
  
)

