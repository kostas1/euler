using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Problem349
{
    class Program
    {
        static Dictionary<Tuple<int, int>, int> map = new Dictionary<Tuple<int, int>, int>();
        static Tuple<int, int> position = new Tuple<int, int>(0, 0);
        static int direction = 1;
        static int totalCount = 0;
        static int totalBlack = 0;
        static List<Tuple<int, int>> function = new List<Tuple<int, int>>();

        static void Main(string[] args)
        {
            //Prelude> 10^18
            //1000000000000000000
            //Prelude> div (10^18-10085) 104
            //9615384615384518
            //Prelude> 10^18-10085 - (104*9615384615384518)
            //43
            //Prelude> (9615384615384518*12) + 725 + 11
            //115384615384614952
            //Prelude>

            // take a safe number (10085), it's value is 725 black squares.
            // find out how many cycles can you do in remaining steps
            // find out the remainder after all the cycles
            // find out the increment after remainder number of steps (+11)
            // multiply number of valid cycles times increment of 12 per cycle, 
            // add 11 for remainder and 725 of initial step
            // where cycle applies

            // cycle is 104 steps
            // cycle increment is +12

            for (int i = 0; i < 11000; i++)
            {
                proceed();
            }
            //saveToFile();
            Console.WriteLine(function[10085-1]);
            Console.WriteLine(function[10189-1]);
        }

        static void saveToFile()
        {
            string filePath = @"C:\Users\kostas\Desktop\results.csv";
            string delimiter = ",";

            int length = function.Count;
            StringBuilder sb = new StringBuilder();
            for (int i = 10000; i < length; i++)
            {
                var item = function[i];
                var line = new string[] { "" + (item.Item1 - 10000), "" + item.Item2 };
                sb.AppendLine(string.Join(delimiter, line));
            }
            File.WriteAllText(filePath, sb.ToString()); 
        }

        #region algorithm
        static void proceed()
        {
            int color = 0;
            map.TryGetValue(position, out color);

            if (color == 1)
            {
                map[position] = 0;
                rotateCC();
                totalBlack -= 1;
            }
            else
            {
                map[position] = 1;
                rotateC();
                totalBlack += 1;
            }
            totalCount += 1;
            moveForward();
            function.Add(new Tuple<int, int>(totalCount, totalBlack));
        }

        static void moveForward()
        {
            if (direction == 1)
                position = new Tuple<int, int>(position.Item1, position.Item2 + 1);
            else if (direction == 2)
                position = new Tuple<int, int>(position.Item1 + 1, position.Item2);
            else if (direction == 3)
                position = new Tuple<int, int>(position.Item1, position.Item2 - 1);
            else if (direction == 4)
                position = new Tuple<int, int>(position.Item1 - 1, position.Item2);
        }

        static void rotateC()
        {
            direction = direction + 1;
            if (direction > 4)
                direction = 1;
        }

        static void rotateCC()
        {
            direction = direction - 1;
            if (direction < 1)
                direction = 4;
        }
        #endregion
    }
}
