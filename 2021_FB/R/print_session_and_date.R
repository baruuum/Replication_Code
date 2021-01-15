#' Print session information, date, and runtime of replication
#' 
#' @param run_start_time a \code{POSIXct}, \code{POSIXt} class object
#' @return prints the current date, session information, and the total runtime (if \code{run_start_time} is not \code{NULL})
print_session_and_date = function(run_start_time = NULL) 
{
    
    if (!is.null(run_start_time) & !all(c("POSIXct", "POSIXt") %in% class(run_start_time)))
        stop("run_start_time is not of class POSIXct, POSIXt")
    
    run_time = if (is.null(run_start_time)) 
        {
            ""
        } else {
            
            paste0(
                "\nTotal Runtime : ",
                sub(
                    "Time difference of ",
                    "", 
                    capture.output(
                        print(
                            difftime(Sys.time(), run_start_time)
                        )
                    )
                )
            )
            
        }
    
    
    
    message(
        paste0(
            "\nDate : ",
            Sys.time(),
            run_time,
            "\n\n",
            "Session information ",
            "-------------------------------------------------------------------"
        ),
        "\n\n",
        paste0(capture.output(print(sessionInfo())), collapse = "\n"),
        "\n\n---------------------------------------------------------------------------------------",
        "\n\n",
        "Greetings from Barum and Siwei!\n",
        "\nWe hope you were able to replicate all results successfully. In case you have any questions regarding the rawdata, direct them to siwei.cheng@nyu.edu. For questions regarding the analysis and models/algorithms, contact bp1094@nyu.edu.",
        "\n\n"
    )
    
}
