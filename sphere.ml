
let unique = 
  let last = ref 0 in 
  fun () -> incr last ; !last

class sphere =
  object(self)
   val _id = unique ()
  method id =
    _id
end