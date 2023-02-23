#pragma once

#include "observer.h"
#include <iostream>
#include <vector>

class ChatSession : public Subject
{
private:
    std::vector<std::pair<std::string, std::string>> messages;

public:
    void addMessage(std::string user, std::string message);
    std::vector<std::pair<std::string, std::string>> getMessages();
};