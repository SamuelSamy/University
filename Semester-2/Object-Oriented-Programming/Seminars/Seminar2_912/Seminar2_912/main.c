#include "UI.h"
#include <crtdbg.h>

int main()
{
	PlanetRepo* repo = createRepo();
	Service* ctrl = createService(repo);

	addPlanetServ(ctrl, "Wolf 1061 c", "terrestrial", 13.8);
	addPlanetServ(ctrl, "HAT-P-26b", "Neptune-like", 450);
	addPlanetServ(ctrl, "Proxima Centauri b", "terrestrial", 4.2);
	addPlanetServ(ctrl, "K2-18b", "super-Earth", 111);

	UI* ui = createUI(ctrl);

	startUI(ui);

	destroyUI(ui);

	_CrtDumpMemoryLeaks();

	return 0;
}