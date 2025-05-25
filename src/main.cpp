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
    if (tmp == "exit")
      return 0;
  }
  return 0;
}
