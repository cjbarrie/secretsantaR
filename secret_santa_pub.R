#define function to get random pairs where everyone gifts one and everyone receives one
#without duplication

secretsantaR <- function(x){
  emails_names <- x
  sampled_names <- sample(x[,2])
  santadf <- data.frame(names = sampled_names,
                        receiver.names = c(sampled_names[-1], 
                                           sampled_names[1]))
  santadf <- santadf %>%
    left_join(emails_names, by="names") %>%
    mutate(giver.names = names) %>%
    select(giver.names, receiver.names, emails)
  return(santadf)
}
#get list of Santa and recipient pairs with emails then names

names <- data.frame(emails =c("email1", "email2",
                              "email3", "email4"),
                    names = c("Name1", "Name2", "Name3", "Name4"))


#check working:
secretsantaR(names)


library(gmailr)

#set up API token for gmail account, following instructions here:
#https://github.com/r-lib/gmailr
gm_auth_configure(path = "PATH_TO_CREDIENTALS/credentials.json")
gm_auth() #authorize token

#get names randomly sorted into pairs
sorted_names <- secretsantaR(names)

for (i in 1:nrow(sorted_names)) {
  recipient.email <- sorted_names[i, "emails"]
  recipient <- sorted_names[i, "receiver.names"]
  giver <- sorted_names[i, "giver.names"]
  
  email_to_send <-
    gm_mime() %>%
    gm_to(recipient.email) %>%
    gm_from("SENDER_ACCOUNT@gmail.com") %>% #use one gmail account to send all messages
    gm_subject("Secret Book email") %>%
    gm_text_body(paste0("Hello and MERRY CHRISTMAS, ", giver, ". Santa is here, and he's here to tell you that you're buying a present for: ",
                       recipient, "! \n 
               HO HO HO MERRY READING!"))
  gm_send_message(email_to_send)
}
