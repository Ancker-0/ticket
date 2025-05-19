#include "cmd.h"

int main() {
  std::string tmp;
  while (std::getline(std::cin, tmp)) {
    if (tmp == "")
      continue;
    dispatch(tmp);
  }
  return 0;
}
