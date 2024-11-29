// #include<elf.h>

int main(int argc, int argv)
{
  int x = 100;
  thisismyfakeconstantbecauseidonthaveconstant(x - 1);
  return argv;
}

int thisismyfakeconstantbecauseidonthaveconstant(int x)
{
  return 1 + 1;
}
