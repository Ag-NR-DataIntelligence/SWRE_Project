EmailCompleteMsg=function(Pass)
{
    library(mailR)
    sender <- "ziwenyu83@gmail.com"
    recipients <- c("ziwenyu83@gmail.com")
    send.mail(from = sender,
              to = recipients,
              subject = "Execution completed",
              body = "Program running completed.",
              smtp = list(host.name = "smtp.gmail.com", port = 465, 
                          user.name = "ziwenyu83@gmail.com",            
                          passwd = Pass, ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
}
