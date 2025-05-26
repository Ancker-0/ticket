#include "cmd.h"

int main() {
#if TESTFILE
  freopen(FILENAME, "r", stdin);
#endif
  std::string tmp;
  while (std::getline(std::cin, tmp)) {
    if (tmp == "")
      continue;
    std::cout << dispatch(tmp) << std::endl;
    if (tmp.find("exit") != tmp.npos) {
      accounter.login_list.clear();
      errf("bye\n");
    }
  }
  return 0;
}
