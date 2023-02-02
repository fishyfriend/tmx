let error err = raise (Error.Error err)
let invalid_arg arg = error (`Invalid_arg arg)
