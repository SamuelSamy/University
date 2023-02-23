#include "chat_session.h"

void ChatSession::addMessage(std::string user, std::string message)
{
    this->messages.push_back(std::make_pair(user, message));
}

std::vector<std::pair<std::string, std::string>> ChatSession::getMessages()
{
    return this->messages;
}
