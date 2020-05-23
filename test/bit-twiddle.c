/* bit-twiddle.c --- 
 * 
 * Filename: bit-twiddle.c
 * Description: 
 * Author: Karthik Kumar
 * Maintainer: 
 * Created: Tue Apr  9 16:56:15 2019 (+0530)
 * Version: 
 * Package-Requires: ()
 * Last-Updated: Mon Apr 15 14:01:25 2019 (+0530)
 *           By: Karthik Kumar
 *     Update #: 36
 * URL: 
 * Doc URL: 
 * Keywords: 
 * Compatibility: 
 * 
 */

/* Commentary: 
 * 
 * 
 * 
 */

/* Change Log:
 * 
 * 
 */

/* This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
 */

/* Code: */

#include <stdio.h>

int main(int argc, char *argv[])
{
    printf("ldb = %d\n", (689>>4)&0x7F);
    printf("128 >> 7 = %d\n", (689>>4));
    printf("dpb = %d\nright = %d\nleft = %d\n0|400 = %d\n",
           (689&(~(0x7F<<4)))|((12&0x1F)<<7), 
           (128&(~(0x1F<<7))), 
           ((12&0x1F)<<7),
           (0|400));


    printf("\n\n\n");
    printf("%d \n%d\n",
           (0x1F<<7),
           (~(0X1f<<7)));
    return 0;
}


/* bit-twiddle.c ends here */
