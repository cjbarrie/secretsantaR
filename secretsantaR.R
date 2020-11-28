library(gmailr)

#define function to get random pairs where everyone gifts one and everyone receives one
#without duplication

secretsantaR <- function(names){
  emails_names <- names
  sampled_names <- sample(names[,2])
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

names <- data.frame(emails =c("chrisjbarrie@googlemail.com", "georgiabarrie@gmail.com",
                  "grahammcmbarrie@hotmail.co.uk", "mandybarrie@hotmail.com"),
                  names = c("Chris", "Georgia", "Graham", "Mandy"))



gm_auth_configure(path = "/Users/christopherbarrie/Dropbox/sandbox/secret_santa/credentials.json")
gm_auth()

sorted_names <- secretsantaR(names)

for (i in 1:nrow(sorted_names)) {
  
  recipient.email <- sorted_names[i, "emails"]
  recipient <- sorted_names[i, "receiver.names"]
  gifter <- sorted_names[i, "giver.names"]
  
  email_to_send <-
    gm_mime() %>%
    gm_to(recipient.email) %>%
    gm_from("chrisjbarrie@gmail.com") %>%
    gm_subject("Secret Book Santa email") %>%
    gm_text_body(paste("Santa is here, and he's here to tell you that you're buying a book for",
                       recipient, "! \n 
               HO HO HO MERRY READING!"))
  gm_send_message(email_to_send)
}
