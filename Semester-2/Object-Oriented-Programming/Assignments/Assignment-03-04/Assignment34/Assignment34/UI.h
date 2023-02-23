#pragma once

#include "Service.h"

typedef struct _UI {
	Service* service;
} UI;


UI* createUI(Service* service);
void destroyUI(UI* ui);

void startUI(UI* ui, int undoType);