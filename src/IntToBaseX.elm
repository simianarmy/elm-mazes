module IntToBaseX exposing (toBaseX)

import String

digitMap = "0123456789abcdefghijklmnopqrstuvwxyz"

lookupDigitChar n =
    String.slice n (n + 1) digitMap

-- Converts number to base X-encoded string
toBaseX : Int -> Int -> String
toBaseX num base =

    let convert : String -> Int -> String
        convert str v =
            if v == 0
               then str
               --else convert ((lookupDigitChar (v % base)) ++ str) (v / toFloat base)
               else
               let c = (lookupDigitChar (v % base)) ++ str
               in
                  convert c (v // base)
    in
       if base < 2 || 36 < base then "illegal radix " ++ (toString base)
          else
          if num == 0 then
             "0"
             else
             let num_ = (if (num < 0) then negate num else num)
                 res = convert "" num_
             in
                if num < 0 then ("-" ++ res) else res
