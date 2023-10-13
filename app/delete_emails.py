import imaplib

# Your credentials
email_user = 'mhegscoreapp@gmail.com'
email_pass = 'kidi gowk ujvv arkl'

# Connect to Gmail and login
mail = imaplib.IMAP4_SSL('imap.gmail.com')
mail.login(email_user, email_pass)

# Select the inbox
mail.select('"[Gmail]/Sent Mail"')

# Search for all emails
status, messages = mail.search(None, "ALL")

# List to keep track of message ids for deletion
delete_ids = []

# Get emails ids
for message_id in messages[0].split():
    delete_ids.append(message_id)

# Delete emails
for delete_id in delete_ids:
    # Mark email for deletion
    mail.store(delete_id , '+FLAGS', '\\Deleted')

# Expunge (permanently remove) emails marked for deletion
mail.expunge()

# Logout and close connection
mail.logout()