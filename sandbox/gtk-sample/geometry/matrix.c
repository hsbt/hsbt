/*
	Matrix tool by jgpi Optimized CG 
	hsbt 2001/6/21
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct { double v[3][3]; } gT_M33;            /* 行列 */
typedef struct { double v[3]; } gT_M31;               /* ベクトル */
typedef struct { double re,im; } gT_CX;               /* 複素数 */

/* 3x3行列に値をセットする関数 */
gT_M33 gM33Set(double v11, double v12, double v13,
               double v21, double v22, double v23,
               double v31, double v32, double v33)
{
	gT_M33 p;

	p.v[0][0] = v11; p.v[0][1] = v12; p.v[0][2] = v13;
	p.v[1][0] = v21; p.v[1][1] = v22; p.v[1][2] = v23;
	p.v[2][0] = v31; p.v[2][1] = v32; p.v[2][2] = v33;
	return p;
}
/* 3x3行列どうしの積m1m2を求める関数 */
gT_M33 gM33xM33(gT_M33 m1, gT_M33 m2)
{
	int i, j, k;
	gT_M33 m;

	for (i = 0; i < 3; i ++) {
		for (j = 0; j < 3; j ++) {
			m.v[i][j] = 0;
			for (k = 0; k < 3; k ++)
				 m.v[i][j] += m1.v[i][k]*m2.v[k][j];
		}
	}
	return m;
}
/* 3x3行列の成分を表示する関数 */
void gM33Disp(gT_M33 m)
{
	int i, j;

	for (i=0; i<3; i++) {
		for (j = 0; j < 3; j ++)
			printf("%8.3lf ", m.v[i][j]);
		putchar('\n');
	}
}
/* 3x3単位行列を返す関数 */
gT_M33 gM33Unit(void)
{
	gT_M33 p={0};

	p.v[0][0]=p.v[1][1]=p.v[2][2]=1.0;
	return p;
}
/* 3x3ゼロ行列を返す関数 */
gT_M33 gM33Zero(void)
{
	gT_M33 p={0};

	return p;
}
/* 3x3行列と3x1行列の積を求める関数 */
gT_M31 gM33xM31(gT_M33 m1, gT_M31 m2)
{
	int i,j;
	gT_M31 m;

	for(i=0;i < 3;i++){
		m.v[i]=0;
		for(j=0;j < 3;j++)
			m.v[i] += m1.v[i][j]*m2.v[j];
	}
	return m;
}
/* 3x1行列に値をセットする関数 */
gT_M31 gM31Set(double v1, double v2, double v3)
{
	gT_M31 p;

	p.v[0] = v1; p.v[1] = v2; p.v[2] = v3;
	return p;
}
