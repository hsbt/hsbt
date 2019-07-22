#include <gtk/gtk.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "bmptype.h"

GdkGC *g_gc = NULL;
GtkWidget *g_window;
//double scale_x=1.0, scale_y=1.0;
int scale_x=1, scale_y=1;
char fname1[128], fname2[128];
FILE *fp1, *fp2;
BMPFILE_HEADER bmpf_h;
BMPINFO_HEADER bmpi_h;
int pict_width, pict_height;
unsigned char bmp_ctable[256][3];
guchar *rgbbuf;
int inbmp_mode;

int count_24table[256][256][256];
int count_8table[256];

int hist_rtable[256];
int cvalue=0;
int size;

gint destroyapp (GtkWidget *, gpointer);
void draw_picture(GdkDrawable *);
gint expose_event(GtkWidget *, GdkEventExpose *);
void read_bmpfile(FILE *), write_bmpfile(FILE *);
char *chk_extention(char *, char *);
FILE *open_bmpfile(char *, BMPFILE_HEADER *, BMPINFO_HEADER *);
FILE *open_bmpfile_w(char *, int, int, int, BMPFILE_HEADER *, BMPINFO_HEADER *);
long int get_data(FILE *, Endian, int);
void put_data(FILE *, Endian, int, long int);
void flatred(void);

int main(int argc, char *argv[])
{
  GtkWidget *window;
  GtkWidget *drawing_area;
  int mode;

  /* --- GTK initialization --- */
  gtk_init( &argc, &argv );
  gdk_rgb_init();

  /* --- Open BMP file and Read headers --- */
  if (argc>1) strcpy(fname1, argv[1]);
  else { printf("入力画像ファイル名： "); gets(fname1); }

  if (argc>2) strcpy(fname2, argv[2]);
  else { printf("出力画像ファイル名： "); gets(fname2); }

  if (argc>3) mode = atoi(argv[3]);
  else {
    printf("何バイト／ピクセルで出力しますか？ [8 or 24] ");
    scanf("%d", &mode);
  }

  fp1=open_bmpfile(fname1, &bmpf_h, &bmpi_h);
  pict_width=bmpi_h.biWidth; pict_height=bmpi_h.biHeight;

  // ピクセルの総数
  size=pict_width*pict_height;
  // 度数の最大数
  cvalue=size/256;

  rgbbuf=(guchar *) malloc(pict_width * pict_height * 3);
  if (rgbbuf==NULL) {
    printf("Can't allocate rgbbuf\n"); exit(1);
  }
  read_bmpfile(fp1);
  flatred();

  fp2=open_bmpfile_w(fname2, pict_width, pict_height, mode, &bmpf_h, &bmpi_h);
  write_bmpfile(fp2);

  /* --- Create the top level window --- */
  g_window = window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  /* --- Show the label  --- */
  gtk_widget_show(window);
  gtk_signal_connect(GTK_OBJECT(window), "destroy",
             GTK_SIGNAL_FUNC(destroyapp), NULL);

  drawing_area = gtk_drawing_area_new();
  gtk_drawing_area_size( GTK_DRAWING_AREA(drawing_area),
             pict_width, pict_height);
  gtk_signal_connect(GTK_OBJECT(drawing_area), "expose_event",
             (GtkSignalFunc) expose_event, NULL);
  gtk_widget_set_events(drawing_area, GDK_EXPOSURE_MASK
    | GDK_BUTTON_PRESS_MASK);
  gtk_container_add(GTK_CONTAINER(window), drawing_area);

  /* --- Show the label  --- */
  gtk_widget_show(drawing_area);

  /* --- Create GC  --- */
  g_gc = gdk_gc_new(window->window);

  gtk_main();
  return 0;
}

gint destroyapp(GtkWidget *widget, gpointer data)
{
  g_print("Quitting ...\n");
  /* --- Shut down gtk event loop processing --- */
  gtk_main_quit ();

  return 0;
}

void read_bmpfile(FILE *fp)
{
  int x, y, ct, i, r, g, b, x2, y2,tp;

  printf("scale: (%d, %d)\n", scale_x, scale_y);
  if (scale_x<scale_y) scale_y=scale_x;
  else scale_x=scale_y;

  for(i=0;i<256;i++)
	hist_rtable[i]=0;

  // 入力BMPファイルの階調度を保存
  // 書き込み処理の際に使用
  inbmp_mode=bmpi_h.biBitCount;

  for (y = 0; y < pict_height; y ++) {
    /* バイト数カウンタのクリア */
    ct = 0;
    for (x = 0; x < pict_width; x ++) {
      switch (bmpi_h.biBitCount) {
      case 24:
	// 24bitの場合はファイルから色情報を直接取り込む
      b = fgetc(fp); g = fgetc(fp); r = fgetc(fp);
        // インデックスをカウント
      count_24table[r][g][b]++;
      hist_rtable[r]++;

break;
      case 8:
      // 8bitの場合はファイルからカラーテーブルのインデックスを取り込む
        tp=fgetc(fp);
        // インデックスをカウント
        count_8table[tp]++;

        // 取り込んだインデックスを使ってカラーテーブルからRGB値を取り出す
        b=bmp_ctable[tp][0];
        g=bmp_ctable[tp][1];
        r=bmp_ctable[tp][2];
      }

      // スケーリング
      x2=x*scale_x; y2=(-y+pict_height-1)*scale_y;

      // RGBバッファにRGB値を格納
      rgbbuf[(x2+y2*pict_width)*3+0] = r;
      rgbbuf[(x2+y2*pict_width)*3+1] = g;
      rgbbuf[(x2+y2*pict_width)*3+2] = b;

      /* バイト数のカウント */
      ct += (bmpi_h.biBitCount/8);
    }

    /* 画面水平１ラインは、必ず４の倍数バイトのデータ */
    ct %= 4;

    /* パディングデータの読みとばし */
    if (ct) for (i = ct; i < 4; i ++) fgetc(fp);
  }

  fclose(fp);
}

void write_bmpfile(FILE *fp)
{
  int x, y, ct, i,j,k, r, g, b, x2, y2;
  int r8,g8,b8;
  int new_index,color_count;


  double c_vector,new_c_vector;

  for (y = 0; y < pict_height; y ++) {

    /* バイト数カウンタのクリア */
    ct = 0;

    for (x = 0; x < pict_width; x ++) {
      // スケーリング
      x2 = x*scale_x; y2 = (-y+pict_height-1)*scale_y;

      // RGBバッファからRGB値を取り出す
      r = rgbbuf[(x2+y2*pict_width)*3+0];
      g = rgbbuf[(x2+y2*pict_width)*3+1];
      b = rgbbuf[(x2+y2*pict_width)*3+2];

      switch (bmpi_h.biBitCount) {
      case 24:
	// 24bitの場合は直接ファイルに出力
	fputc(b, fp); fputc(g, fp); fputc(r, fp);
	break;
      case 8:
	// 8bitの場合はカラーテーブルのインデックスを出力
	switch(inbmp_mode){
	case 24:
	  // とりあえず黒を初期値とする
	  new_index=0;
	  b8=bmp_ctable[0][0];
	  g8=bmp_ctable[0][1];
	  r8=bmp_ctable[0][2];

	  // 黒と24bitカラーの色の色空間距離を算出
	  c_vector=pow((double)(b-b8),2.0)
	    +pow((double)(g-g8),2.0)+pow((double)(r-r8),2.0);

	  for(i=1;i<256;i++){
	    // カラーテーブルから色を取り出す
	    b8=bmp_ctable[i][0];
	    g8=bmp_ctable[i][1];
	    r8=bmp_ctable[i][2];

	    // 取り出した色と24bitカラーの色空間距離を算出
	    new_c_vector=pow((double)(b-b8),2.0)
	      +pow((double)(g-g8),2.0)+pow((double)(r-r8),2.0);

	    // 取り出した色の色空間距離が短ければその色を使う
	    if(new_c_vector < c_vector){
	      new_index = i;
	      c_vector=new_c_vector;
	    }
	  }

	  // 選択された色のインデックスの出力
	  fputc(new_index,fp);
	  break;
	case 8:
	  // 入力が8bitの場合はカラーテーブルの中で
	  // 完全一致するインデックスを出力
	  for(i=0;i<256;i++)
	    if(b==bmp_ctable[i][0] && g==bmp_ctable[i][1] && r==bmp_ctable[i][2])
	      fputc(i,fp);
	}
      }
      /* バイト数のカウント */
      ct += (bmpi_h.biBitCount/8);
    }

    /* 画面水平１ラインは、必ず４の倍数バイトのデータ */
    ct %= 4;

    /* パディングデータの書き込み */
    if (ct) for (i = ct; i < 4; i ++) fputc(0, fp);
  }

  switch(inbmp_mode){
  case 24:
    for(i=0;i<256;i++)
      for(j=0;j<256;j++)
	for(k=0;k<256;k++)
	  if( count_24table[i][j][k] > 0)
	    color_count++;
    break;
  case 8:
    for(i=0;i<256;i++)
      if( count_8table[i] > 0)
	color_count ++;
  }

  printf("使用している色数：%d\n",color_count);

  fclose(fp);
}

void draw_picture(GdkDrawable *gd)
{
  gdk_draw_rgb_image (gd, g_gc,
              0, 0, pict_width, pict_height,
              GDK_RGB_DITHER_MAX, rgbbuf, pict_width * 3);
}
gint expose_event(GtkWidget *widget, GdkEventExpose *event)
{
  draw_picture(widget->window);
  return FALSE;
}

// Windowsビットマップファイルのオープン（読み込み）
FILE *open_bmpfile(char *file, BMPFILE_HEADER *bmph, BMPINFO_HEADER *bmpi)
{
  FILE *fp;
  int i,j,k;
  char *fname;

  fname=chk_extention(file, "bmp");
  if ((fp=fopen(fname, "rb")) == NULL) {
    fprintf(stderr, "\"%s\" をオープンできません。", fname);
    exit(1);
  }

  //   ビットマップファイルヘッダーの読み込み
  bmph->type = get_data(fp, Little, 2);        // 2
  bmph->size = get_data(fp, Little, 4);        // 4
  bmph->rsv1 = get_data(fp, Little, 2);        // 2
  bmph->rsv2 = get_data(fp, Little, 2);        // 2
  bmph->ofsbits = get_data(fp, Little, 4);    // 4  ( sum : 14 )

  // ビットマップインフォヘッダーの読み込み
  bmpi->biSize = get_data(fp, Little, 4);          // 4
  bmpi->biWidth = get_data(fp, Little, 4);         // 4
  bmpi->biHeight = get_data(fp, Little, 4);        // 4
  bmpi->biPlanes = get_data(fp, Little, 2);        // 2
  bmpi->biBitCount = get_data(fp, Little, 2);      // 2
  bmpi->biCompression = get_data(fp, Little, 4);   // 4
  bmpi->biSizeImage = get_data(fp, Little, 4);     // 4
  bmpi->biXPelsPerMeter = get_data(fp, Little, 4); // 4
  bmpi->biYPelsPerMeter = get_data(fp, Little, 4); // 4
  bmpi->biClrUsed = get_data(fp, Little, 4);       // 4
  bmpi->biClrImportant = get_data(fp, Little, 4);  // 4  ( sum : 40 )
  // ************* Total 54 bytes *****************

  printf("画像サイズ：%5ld×%5ld\n", bmpi->biWidth, bmpi->biHeight);
  printf("入力ビットマップデータ：%2d bit/pixel\n", bmpi->biBitCount);

  // カラーテーブルの読み込み（8 bit/pixelの場合のみ）
  if (bmpi->biBitCount == 8) {
    for (i = 0; i < 256; i ++) {
      bmp_ctable[i][0] = fgetc(fp);
      bmp_ctable[i][1] = fgetc(fp);
      bmp_ctable[i][2] = fgetc(fp);
      fgetc(fp);
    }
  }
  // カウントテーブルの初期化
  if (bmpi->biBitCount == 8) {
    for(i=0;i<256;i++)
      count_8table[i]=0;
  }else{
    for(i=0;i<256;i++)
      for(j=0;j<256;j++)
        for(k=0;k<256;k++)
    	  count_24table[i][j][k]=0;
  }
  return fp;
}

// Windowsビットマップファイルのオープン（書き込み）
FILE *open_bmpfile_w(char *file, int xsize, int ysize, int mode, BMPFILE_HEADER *bmph, BMPINFO_HEADER *bmpi)
{
  FILE *fp;
  int i,j,k;
  long int lsize;
  char *fname;

  fname=chk_extention(file, "bmp");
  if ((fp=fopen(fname, "wb"))==NULL) {
    fprintf(stderr, "\"%s\" をオープンできません。\n", fname); exit(1);
  }

  /* 画面水平１ライン当たりのビットマップデータのバイト数 */
  lsize = xsize * mode/8 + ((4-((xsize * mode/8) % 4)) % 4);

  /* ビットマップファイルヘッダーの設定 */
  bmph->type = 0x4D42;                         /* "BM" */
  bmph->ofsbits = 54L +(mode==24 ? 0L : 1024L);
  bmph->size = bmph->ofsbits + ysize * lsize;
  bmph->rsv1 = bmph->rsv2 = 0;

  /* ビットマップインフォヘッダーの設定 */
  bmpi->biSize = 40;
  bmpi->biWidth = xsize;
  bmpi->biHeight = ysize;
  bmpi->biPlanes = 1;
  bmpi->biBitCount = mode;
  bmpi->biCompression = 0L;
  bmpi->biSizeImage = 0L;
  bmpi->biXPelsPerMeter = 0L;
  bmpi->biYPelsPerMeter = 0L;
  bmpi->biClrUsed = 0L;
  bmpi->biClrImportant = 0L;

  /* ビットマップファイルヘッダーの書き込み */
  put_data(fp, Little, 2, bmph->type);    /* 2 */
  put_data(fp, Little, 4, bmph->size);    /* 4 */
  put_data(fp, Little, 2, bmph->rsv1);    /* 2 */
  put_data(fp, Little, 2, bmph->rsv2);    /* 2 */
  put_data(fp, Little, 4, bmph->ofsbits); /* 4 ( sum : 14 ) */

  /* ビットマップインフォヘッダーの書き込み */
  put_data(fp, Little, 4, bmpi->biSize);          /* 4 */
  put_data(fp, Little, 4, bmpi->biWidth);         /* 4 */
  put_data(fp, Little, 4, bmpi->biHeight);        /* 4 */
  put_data(fp, Little, 2, bmpi->biPlanes);        /* 2 */
  put_data(fp, Little, 2, bmpi->biBitCount);      /* 2 */
  put_data(fp, Little, 4, bmpi->biCompression);   /* 4 */
  put_data(fp, Little, 4, bmpi->biSizeImage);     /* 4 */
  put_data(fp, Little, 4, bmpi->biXPelsPerMeter); /* 4 */
  put_data(fp, Little, 4, bmpi->biYPelsPerMeter); /* 4 */
  put_data(fp, Little, 4, bmpi->biClrUsed);       /* 4 */
  put_data(fp, Little, 4, bmpi->biClrImportant);  /* 4  ( sum : 40 ) */
  /************* Total 54 bytes *****************/

  if (mode == 8) {
    // カラーテーブルの作成
    switch(inbmp_mode){
    case 24:
      // 入力BMPファイルが24bitの場合はカラーテーブルを
      // 作り直す

      // 0-215にRGB6段階216色
      for (i=0;i<6;i++)
    for (j=0;j<6;j++)
      for (k=0;k<6;k++) {
        bmp_ctable[i*36+j*6+k][0]=i*51;
        bmp_ctable[i*36+j*6+k][1]=j*51;
        bmp_ctable[i*36+j*6+k][2]=k*51;
          }
      // 216-222は使わない範囲なのでとりあえず黒にしておく
      for(i=216;i<222;i++){
        bmp_ctable[i][0]=0;
        bmp_ctable[i][1]=0;
        bmp_ctable[i][2]=0;
      }
      // 223-254に32段階のグレースケール
      for (i=0;i<32;i++) {
        bmp_ctable[i+223][0]=i*8;
        bmp_ctable[i+223][1]=i*8;
        bmp_ctable[i+223][2]=i*8;
      }
      // 最後は白を作る
      bmp_ctable[255][0]=255;
      bmp_ctable[255][1]=255;
      bmp_ctable[255][2]=255;

      // カラーテーブルの書き込み
      for (i = 0; i < 256; i ++) {
        fputc(bmp_ctable[i][0],fp);
        fputc(bmp_ctable[i][1],fp);
        fputc(bmp_ctable[i][2],fp);
        // バイトを合わせるためにインデックスを書き込む
        fputc(i,fp);
      }
      break;

    case 8:
      // 入力BMPファイルが8bitの場合はカラーテーブルを
      // 変更しないで書き込む
      for (i = 0; i < 256; i ++) {
        fputc(bmp_ctable[i][0],fp);
        fputc(bmp_ctable[i][1],fp);
        fputc(bmp_ctable[i][2],fp);
        // バイトを合わせるためにインデックスを書き込む
        fputc(i,fp);
      }
    }
  }
  return fp;
}

// ファイル名の拡張子をチェックする関数
char *chk_extention(char *file, char *ext)
{
  static char fname[80], *p;

  strcpy(fname, file);
  p=fname;
  while (*p!='\0') {
    if (*p++ == '.') return fname;
  }
  strcat(fname, "."); strcat(fname, ext);
  return fname;
}

long int get_data(FILE *fp, Endian ed, int n)
{
  unsigned char c[4];
  long int ldata=0;
  int i, p;

  for (i=0; i<n; i++) c[i]=fgetc(fp);

  if (ed==Little) p=0;
  else p=n-1;

  for (i=0; i<n; i++) {
    ldata |= c[p]<<8*i;
    p+=ed;
  }

  return(ldata);
}

void put_data(FILE *fp, Endian ed, int n, long int ldata)
{
  unsigned char c[4];
  int i, p;

  if (ed==Little) p=0;
  else p=n-1;

  for (i=0; i<n; i++) {
    c[p] = (ldata>>8*i) & 0xff;
    p+=ed;
  }

  for (i=0; i<n; i++) fputc(c[i], fp);
}
void flatred(void)
{
  int x, y, x2, y2,r,r2,r3;
  int flag;
  int limit=cvalue;

  for(r=0;r<256;r++){
    flag=1;
    if(hist_rtable[r] < limit){
      for (y = 0; y < pict_height && flag ; y ++) {
	for (x = 0; x < pict_width && flag ; x ++) {
	  x2 = x*scale_x; y2 = (-y+pict_height-1)*scale_y;
	  r2=rgbbuf[(x2+y2*pict_width)*3+0];
	  //	  if(hist_rtable[r2] > 0){
	    hist_rtable[r]++;
	    hist_rtable[r2]--;
	    rgbbuf[(x2+y2*pict_width)*3+0]=r;
	    //	  }
	  if(hist_rtable[r]==limit)
	    flag=0;
	}
      }
    }
    if(hist_rtable[r] > limit){
      for (y = 0; y < pict_height && flag ; y ++) {
	for (x = 0; x < pict_width && flag; x ++) {
	  x2 = x*scale_x; y2 = (-y+pict_height-1)*scale_y;
	  r2=rgbbuf[(x2+y2*pict_width)*3+0];
	  if(r2==r){
	  for(r3=255;hist_rtable[r3]>=limit;r3--);
	  hist_rtable[r]--;
	  hist_rtable[r3]++;
	  rgbbuf[(x2+y2*pict_width)*3+0]=r3;
	  }
	  if(hist_rtable[r]==256)
	    flag=0;
	}
      }
    }
    printf("%d %d\n",r,hist_rtable[r]);
  }
}



