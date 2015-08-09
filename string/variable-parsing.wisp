(ns atomita.wisp.macros.string.variable-parsing
  "Variable parsing in string")

(defmacro $
  [value]
  (if (string? value)
    (let [reg #"([\s\S]*?)(\\*)(\${([a-zA-Z\-*\$][\w.\-*\$]*)})([\s\S]*)"]
      (loop [v (str value)
             f `(+)]
        (if (or (nil? v) (= 0 v.length))
          f
          ;else
          (let [m (.match v reg)]
            (if (nil? m)
              (concat f `(~(String. v)))
              ;else
              (if (= 1 (mod (.-length (aget m 2)) 2))
                (recur (aget m 5) (concat f `(~(String. (+ (aget m 1) (aget m 2) (aget m 3))))))
                ;else
                (recur (aget m 5) (concat f `(~(String. (+ (aget m 1) (aget m 2))) ~(symbol (aget m 4)))))
                )))))))))
