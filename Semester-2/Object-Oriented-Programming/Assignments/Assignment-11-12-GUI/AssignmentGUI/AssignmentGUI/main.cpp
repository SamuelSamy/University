#include "everything.h"
#include "GUI.h"
#include "MainGUI.h"
#include "AdminGUI.h"
#include "admin_repository_file.h"
#include "admin_repository_html.h"
#include "user_service.h"
#include "admin_repository_csv.h"
#include "UserGUI.h"


int main(int argc, char *argv[])
{
	QApplication a(argc, argv);


	MainGUI mainGUI;
	mainGUI.exec();
	std::string userMode = mainGUI.getUserMode();

	FileRepository fileRepo{ "data.txt" };
	fileRepo.read();
	AdminService adminService{ &fileRepo };
	AdminGUI gui{ adminService };

	if (userMode == "admin")
	{
		gui.show();
		return a.exec();

	}

	// user mode
	RepositoryOptionGUI repoOptionGUI;
	repoOptionGUI.exec();
	std::string repoOption = repoOptionGUI.getOption();

	HTMLRepository userHTMLRepo{ "data.html", &fileRepo };
	UserService userHTMLService{ &userHTMLRepo };
	UserGUI userHTMLGui{ userHTMLService, adminService };

	CSVRepository userCSVRepo{ "data.csv", &fileRepo };
	UserService userCSVService{ &userCSVRepo };
	UserGUI userCSVGui{ userCSVService, adminService };

	if (repoOption == "html")
	{
		userHTMLGui.show();
		return a.exec();
	}
		
	userCSVGui.show();
	return a.exec();
}
