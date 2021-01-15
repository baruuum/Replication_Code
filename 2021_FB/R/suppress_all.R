#' Evaluate expression suppressing any output
#' 
#' Evaluates expression suppressing **any** output, even the output created by internal compiled code and error messages. This function should be used with special care.
#' 
#' @param expr any expression
#' @return Evaluates expression without showing any messages/output
suppress_all = function(expr) {
    
    out_file = if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
    suppressWarnings(utils::capture.output(expr, file = out_file, type = "message"))
    
}
