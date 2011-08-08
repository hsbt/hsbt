#include <gtk/gtk.h>
#include "matrix.c"
#include <time.h>

#define PICT_WIDTH   300
#define PICT_HEIGHT  300

GdkGC *g_gc = NULL;
GtkWidget *g_window;

int dipth=10;
double r=0.6,dg=30;

double WIDTH,HEIGHT,OriginX,OriginY,ScaleX,ScaleY;

enum { false, true };

gint destroyapp (GtkWidget *, gpointer);
void SetColor(gushort, gushort, gushort);
void draw_picture(GdkDrawable *);
int self(GdkDrawable *,gT_M31,double,double,int);
gint expose_event(GtkWidget *, GdkEventExpose *);

int main(int argc, char *argv[])
{
  GtkWidget *window;
  GtkWidget *drawing_area;

  if(argc == 4){
   dipth=atoi(argv[1]);
   r=atof(argv[2]);
   dg=atof(argv[3]);
  }

  dg*=(M_PI/180.0);

  /* --- GTK initialization --- */
  gtk_init( &argc, &argv );

  /* --- Create the top level window --- */
  g_window = window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  /* --- Show the label  --- */
  gtk_widget_show(window);
  gtk_signal_connect(GTK_OBJECT(window), "destroy",
         GTK_SIGNAL_FUNC(destroyapp), NULL);

  drawing_area = gtk_drawing_area_new();
  gtk_drawing_area_size( GTK_DRAWING_AREA(drawing_area),
       PICT_WIDTH, PICT_HEIGHT);
  gtk_signal_connect(GTK_OBJECT(drawing_area), "expose_event",
         (GtkSignalFunc) expose_event, NULL);
  gtk_widget_set_events(drawing_area, GDK_EXPOSURE_MASK);
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
  /* --- Shut down gtk event loop processing --- */
  gtk_main_quit ();
}

void SetColor(gushort r, gushort g, gushort b)
{
  GdkColor  color;

  color.red = r, color.green = g, color.blue = b;
  gdk_color_alloc(gdk_colormap_get_system(),&color);
  gdk_gc_set_foreground(g_gc, &color);
}

void draw_picture(GdkDrawable *gd)
{
  double Max_X,Min_X,Max_Y,Min_Y,l=40;
  gT_M31 p;
  
  Max_X=75;
  Min_X=-75;
  Max_Y=75;
  Min_Y=-75;
  
  // 論理座標系の描画領域
  WIDTH=Max_X-Min_X;
  HEIGHT=Max_Y-Min_Y;
  
  // 論理座標系の中心座標
  OriginX=WIDTH/2.0-(Max_X+Min_X)/2;
  OriginY=HEIGHT/2.0+(Max_Y+Min_Y)/2;
  
    // スケールファクタの計算
  ScaleX=(double)PICT_WIDTH/WIDTH,
  ScaleY=(double)PICT_HEIGHT/HEIGHT;

  // 初期値の設定
  p=gM31Set(0,-75,1);

  /* 背景を白に設定 */
  SetColor( 0xffff, 0xffff, 0xffff);
  gdk_gc_set_line_attributes( g_gc, 1, GDK_LINE_SOLID,
           GDK_CAP_BUTT, GDK_JOIN_ROUND );
  gdk_draw_rectangle(gd,g_gc,TRUE,0,0,300,510);

  // 点の色を青に設定
  SetColor( 0x0000, 0x0000, 0xffff);
  gdk_gc_set_line_attributes( g_gc, 1, GDK_LINE_SOLID,
           GDK_CAP_BUTT, GDK_JOIN_ROUND );

  // 幹の描画
  gdk_draw_line(gd,g_gc,(OriginX+p.v[0])*ScaleX,(OriginY-p.v[1])*ScaleY,
                             (OriginX+p.v[0])*ScaleX,(OriginY-p.v[1]-l)*ScaleY);
  self(gd,p,l,0,dipth);
}
int self(GdkDrawable *gd,gT_M31 p,double l,double rad,int n)
{
  gT_M31 new1,new2,old;
  gT_M33 mt1,mt2,x,y;
  
  mt1=gM33Set(cos(rad+dg),-sin(rad+dg),0,sin(rad+dg),cos(rad+dg),0,0,0,1);
  mt2=gM33Set(cos(rad-dg),-sin(rad-dg),0,sin(rad-dg),cos(rad-dg),0,0,0,1);
  
  if (n==0)
    return 0;

  p.v[1]+=l;
  old=p;
  p.v[1]+=(l*r);
  
  /* 座標変換関数 */
  x=gM33Set(1,0,-old.v[0],0,1,-old.v[1],0,0,1);
  y=gM33Set(1,0,old.v[0],0,1,old.v[1],0,0,1);

  /* 座標変換関数の合成 */
  mt1=gM33xM33(gM33xM33(y,mt1),x);
  mt2=gM33xM33(gM33xM33(y,mt2),x);

  new1=gM33xM31(mt1,p);
  new2=gM33xM31(mt2,p);
  
  if(n!=dipth){
    old.v[1]-=l;
    new1.v[1]-=l;
    new2.v[1]-=l;
  }

  rad += dg;
  
  n--;

  gdk_draw_line(gd,g_gc,(OriginX+old.v[0])*ScaleX,(OriginY-old.v[1])*ScaleY,
                          (OriginX+new1.v[0])*ScaleX,(OriginY-new1.v[1])*ScaleY);
  self(gd,new1,l*r,rad,n);
  
  rad -= dg*2;
  
  gdk_draw_line(gd,g_gc,(OriginX+old.v[0])*ScaleX,(OriginY-old.v[1])*ScaleY,
                          (OriginX+new2.v[0])*ScaleX,(OriginY-new2.v[1])*ScaleY);
  self(gd,new2,l*r,rad,n);
  
}
gint expose_event(GtkWidget *widget, GdkEventExpose *event)
{
  draw_picture(widget->window);
  return FALSE;
}
