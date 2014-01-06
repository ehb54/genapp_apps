#include "sassie.h"

QPalette palette_app;
QPalette palette_pb_hover;
QPalette palette_lbl_hover;

mQLabel::mQLabel( QWidget *parent, const char * name ) : QLabel( parent, name ) 
{ 
   mbuddy       = (mQLabel*)0;
   pixmap_base  = (QPixmap*)0;
   pixmap_hover = (QPixmap*)0;
}

mQLabel::mQLabel( const QString & text, QWidget *parent, const char * name ) : QLabel( text, parent, name ) 
{ 
   mbuddy       = (mQLabel*)0;
   pixmap_base  = (QPixmap*)0;
   pixmap_hover = (QPixmap*)0;
}

mQLabel::~mQLabel() {}

void mQLabel::mousePressEvent ( QMouseEvent *e )
{
   QLabel::mousePressEvent( e );
   emit( pressed() );
}

void mQLabel::enterEvent ( QEvent *e )
{
   //   setCursor( Qt::PointingHandCursor );
   if ( mbuddy )
   {
      mbuddy->setPalette( palette_lbl_hover );
      if ( mbuddy->pixmap_hover )
      {
         mbuddy->setPixmap( *(mbuddy->pixmap_hover) );
      }
   } else {
      setPalette( palette_lbl_hover ) ;
   }
   if ( pixmap_hover )
   {
      setPixmap( *pixmap_hover );
   }
   QLabel::enterEvent( e );
}

void mQLabel::leaveEvent ( QEvent *e )
{
   //   unsetCursor();
   if ( mbuddy )
   {
      mbuddy->setPalette( palette_app );
      if ( mbuddy->pixmap_base )
      {
         mbuddy->setPixmap( *(mbuddy->pixmap_base) );
      }
   } else {
      setPalette( palette_app ) ;
   }
   if ( pixmap_base )
   {
      setPixmap( *pixmap_base );
   }
   QLabel::leaveEvent( e );
}


mQPushButton::mQPushButton( QWidget *parent, const char * name ) : QPushButton( parent, name ) { }

mQPushButton::~mQPushButton() {}

void mQPushButton::enterEvent ( QEvent *e )
{
   // setCursor( Qt::PointingHandCursor );
   setPalette( palette_pb_hover );
   QPushButton::enterEvent( e );
}

void mQPushButton::leaveEvent ( QEvent *e )
{
   // unsetCursor();
   setPalette( palette_app );
   QPushButton::leaveEvent( e );
}

mQRadioButton::mQRadioButton( QWidget *parent, const char * name ) : QRadioButton( parent, name ) { }

mQRadioButton::~mQRadioButton() {}

sassie::sassie() : QFrame()
{
   setCaption( "SASSIE 2.0" );
   QLabel *title = new QLabel( "<h2>SASSIE 2.0</h2>", this );
   title->setAlignment( Qt::AlignTop | Qt::AlignHCenter);
   QGridLayout* gl_top = new QGridLayout( 0 );

   QColorGroup cg_le = QColorGroup( 
                                    QColor( Qt::red ),
                                    QColor( Qt::darkBlue ),
                                    QColor( Qt::gray ),
                                    QColor( Qt::darkGray ),
                                    QColor( Qt::cyan ),
                                    QColor( Qt::black ),
                                    QColor( Qt::gray ),
                                    QColor( 220,210,210 ),
                                    QColor( Qt::darkRed ) );

   palette_le = new QPalette( cg_le, cg_le, cg_le );

   QColorGroup cg_cb = QColorGroup( 
                                    QColor( Qt::red ),
                                    QColor( 220,210,210 ),
                                    QColor( Qt::gray ),
                                    QColor( Qt::darkGray ),
                                    QColor( Qt::black ),
                                    QColor( Qt::yellow ),
                                    QColor( Qt::gray ),
                                    QColor( Qt::white ),
                                    QColor( 0,0,95 )
                                    );

   palette_cb = new QPalette( cg_cb, cg_cb, cg_cb );

   QColorGroup cg_lbl_error = QColorGroup( 
                                          QColor( Qt::red ),
                                          QColor( 255,0,0 ),
                                          QColor( Qt::gray ),
                                          QColor( Qt::darkGray ),
                                          QColor( Qt::cyan ),
                                          QColor( Qt::black ),
                                          QColor( Qt::gray ),
                                          QColor( Qt::darkBlue ),
                                          QColor( 0,0,95 )
                                           );

   palette_lbl_error = new QPalette( cg_lbl_error, cg_lbl_error, cg_lbl_error );

   QColorGroup cg_plot = QColorGroup( 
                                     QColor( 220,210,210 ),
                                     QColor( Qt::yellow ),
                                     // next 2 are the box colors
                                     QColor( 0,0,95 ),
                                     QColor( 0,0,95 ),
                                     QColor( Qt::cyan ),
                                     QColor( Qt::green ),
                                     QColor( Qt::red ),
                                     QColor( Qt::green ),
                                     QColor( 0,0,95 )
                                      );

   palette_plot = new QPalette( cg_plot, cg_plot, cg_plot );


   palette_pb_hover = palette_app;
   //   palette_pb_hover.setColor( QColorGroup::Foreground, QColor( 200,128,0 ) );
   // palette_pb_hover.setColor( QColorGroup::Background, Qt::red );
   // palette_pb_hover.setColor( QColorGroup::ButtonText, Qt::blue );
   // palette_pb_hover.setColor( QColorGroup::Foreground, Qt::green );
   palette_pb_hover.setColor( QColorGroup::Button, QColor( 228,255,250 ) );

   palette_lbl_hover = palette_app;
   palette_lbl_hover.setColor( QColorGroup::Foreground, QColor( 200,128,0 ) );

   QColor bgc = QColor( 0,0,95 );

   push_back_color_if_ok( bgc, Qt::yellow );
   push_back_color_if_ok( bgc, Qt::green );
   push_back_color_if_ok( bgc, Qt::cyan );
   push_back_color_if_ok( bgc, Qt::blue );
   push_back_color_if_ok( bgc, Qt::red );
   push_back_color_if_ok( bgc, Qt::magenta );
   push_back_color_if_ok( bgc, Qt::darkYellow );
   push_back_color_if_ok( bgc, Qt::darkGreen );
   push_back_color_if_ok( bgc, Qt::darkCyan );
   push_back_color_if_ok( bgc, Qt::darkBlue );
   push_back_color_if_ok( bgc, Qt::darkRed );
   push_back_color_if_ok( bgc, Qt::darkMagenta );
   push_back_color_if_ok( bgc, Qt::white );
   push_back_color_if_ok( bgc, QColor( 240, 248, 255 ) ); /* Alice Blue */
   push_back_color_if_ok( bgc, QColor( 250, 235, 215 ) ); /* Antique White */
   push_back_color_if_ok( bgc, QColor( 0, 255, 255 ) ); /* Aqua* */
   push_back_color_if_ok( bgc, QColor( 127, 255, 212 ) ); /* Aquamarine */
   push_back_color_if_ok( bgc, QColor( 240, 255, 255 ) ); /* Azure */
   push_back_color_if_ok( bgc, QColor( 245, 245, 220 ) ); /* Beige */
   push_back_color_if_ok( bgc, QColor( 255, 228, 196 ) ); /* Bisque */
   push_back_color_if_ok( bgc, QColor( 0, 0, 0 ) ); /* Black* */
   push_back_color_if_ok( bgc, QColor( 255, 235, 205 ) ); /* Blanched Almond */
   push_back_color_if_ok( bgc, QColor( 0, 0, 255 ) ); /* Blue* */
   push_back_color_if_ok( bgc, QColor( 138, 43, 226 ) ); /* Blue-Violet */
   push_back_color_if_ok( bgc, QColor( 165, 42, 42 ) ); /* Brown */
   push_back_color_if_ok( bgc, QColor( 222, 184, 135 ) ); /* Burlywood */
   push_back_color_if_ok( bgc, QColor( 95, 158, 160 ) ); /* Cadet Blue */
   push_back_color_if_ok( bgc, QColor( 127, 255, 0 ) ); /* Chartreuse */
   push_back_color_if_ok( bgc, QColor( 210, 105, 30 ) ); /* Chocolate */
   push_back_color_if_ok( bgc, QColor( 255, 127, 80 ) ); /* Coral */
   push_back_color_if_ok( bgc, QColor( 100, 149, 237 ) ); /* Cornflower Blue */
   push_back_color_if_ok( bgc, QColor( 255, 248, 220 ) ); /* Cornsilk */
   push_back_color_if_ok( bgc, QColor( 0, 255, 255 ) ); /* Cyan */
   push_back_color_if_ok( bgc, QColor( 0, 0, 139 ) ); /* Dark Blue */
   push_back_color_if_ok( bgc, QColor( 0, 139, 139 ) ); /* Dark Cyan */
   push_back_color_if_ok( bgc, QColor( 184, 134, 11 ) ); /* Dark Goldenrod */
   push_back_color_if_ok( bgc, QColor( 169, 169, 169 ) ); /* Dark Gray */
   push_back_color_if_ok( bgc, QColor( 0, 100, 0 ) ); /* Dark Green */
   push_back_color_if_ok( bgc, QColor( 189, 183, 107 ) ); /* Dark Khaki */
   push_back_color_if_ok( bgc, QColor( 139, 0, 139 ) ); /* Dark Magenta */
   push_back_color_if_ok( bgc, QColor( 85, 107, 47 ) ); /* Dark Olive Green */
   push_back_color_if_ok( bgc, QColor( 255, 140, 0 ) ); /* Dark Orange */
   push_back_color_if_ok( bgc, QColor( 153, 50, 204 ) ); /* Dark Orchid */
   push_back_color_if_ok( bgc, QColor( 139, 0, 0 ) ); /* Dark Red */
   push_back_color_if_ok( bgc, QColor( 233, 150, 122 ) ); /* Dark Salmon */
   push_back_color_if_ok( bgc, QColor( 143, 188, 143 ) ); /* Dark Sea Green */
   push_back_color_if_ok( bgc, QColor( 72, 61, 139 ) ); /* Dark Slate Blue */
   push_back_color_if_ok( bgc, QColor( 47, 79, 79 ) ); /* Dark Slate Gray */
   push_back_color_if_ok( bgc, QColor( 0, 206, 209 ) ); /* Dark Turquoise */
   push_back_color_if_ok( bgc, QColor( 148, 0, 211 ) ); /* Dark Violet */
   push_back_color_if_ok( bgc, QColor( 255, 20, 147 ) ); /* Deep Pink */
   push_back_color_if_ok( bgc, QColor( 0, 191, 255 ) ); /* Deep Sky Blue */
   push_back_color_if_ok( bgc, QColor( 105, 105, 105 ) ); /* Dim Gray */
   push_back_color_if_ok( bgc, QColor( 30, 144, 255 ) ); /* Dodger Blue */
   push_back_color_if_ok( bgc, QColor( 178, 34, 34 ) ); /* Firebrick */
   push_back_color_if_ok( bgc, QColor( 255, 250, 240 ) ); /* Floral White */
   push_back_color_if_ok( bgc, QColor( 34, 139, 34 ) ); /* Forest Green */
   push_back_color_if_ok( bgc, QColor( 255, 0, 255 ) ); /* Fuschia* */
   push_back_color_if_ok( bgc, QColor( 220, 220, 220 ) ); /* Gainsboro */
   push_back_color_if_ok( bgc, QColor( 255, 250, 250 ) ); /* Ghost White */
   push_back_color_if_ok( bgc, QColor( 255, 215, 0 ) ); /* Gold */
   push_back_color_if_ok( bgc, QColor( 218, 165, 32 ) ); /* Goldenrod */
   push_back_color_if_ok( bgc, QColor( 128, 128, 128 ) ); /* Gray* */
   push_back_color_if_ok( bgc, QColor( 0, 128, 0 ) ); /* Green* */
   push_back_color_if_ok( bgc, QColor( 173, 255, 47 ) ); /* Green-Yellow */
   push_back_color_if_ok( bgc, QColor( 240, 255, 240 ) ); /* Honeydew */
   push_back_color_if_ok( bgc, QColor( 255, 105, 180 ) ); /* Hot Pink */
   push_back_color_if_ok( bgc, QColor( 205, 92, 92 ) ); /* Indian Red */
   push_back_color_if_ok( bgc, QColor( 255, 255, 240 ) ); /* Ivory */
   push_back_color_if_ok( bgc, QColor( 240, 230, 140 ) ); /* Khaki */
   push_back_color_if_ok( bgc, QColor( 230, 230, 250 ) ); /* Lavender */
   push_back_color_if_ok( bgc, QColor( 255, 240, 245 ) ); /* Lavender Blush */
   push_back_color_if_ok( bgc, QColor( 124, 252, 0 ) ); /* Lawn Green */
   push_back_color_if_ok( bgc, QColor( 255, 250, 205 ) ); /* Lemon Chiffon */
   push_back_color_if_ok( bgc, QColor( 173, 216, 230 ) ); /* Light Blue */
   push_back_color_if_ok( bgc, QColor( 240, 128, 128 ) ); /* Light Coral */
   push_back_color_if_ok( bgc, QColor( 224, 255, 255 ) ); /* Light Cyan */
   push_back_color_if_ok( bgc, QColor( 238, 221, 130 ) ); /* Light Goldenrod */
   push_back_color_if_ok( bgc, QColor( 250, 250, 210 ) ); /* Light Goldenrod Yellow */
   push_back_color_if_ok( bgc, QColor( 211, 211, 211 ) ); /* Light Gray */
   push_back_color_if_ok( bgc, QColor( 144, 238, 144 ) ); /* Light Green */
   push_back_color_if_ok( bgc, QColor( 255, 182, 193 ) ); /* Light Pink */
   push_back_color_if_ok( bgc, QColor( 255, 160, 122 ) ); /* Light Salmon */
   push_back_color_if_ok( bgc, QColor( 32, 178, 170 ) ); /* Light Sea Green */
   push_back_color_if_ok( bgc, QColor( 135, 206, 250 ) ); /* Light Sky Blue */
   push_back_color_if_ok( bgc, QColor( 132, 112, 255 ) ); /* Light Slate Blue */
   push_back_color_if_ok( bgc, QColor( 119, 136, 153 ) ); /* Light Slate Gray */
   push_back_color_if_ok( bgc, QColor( 176, 196, 222 ) ); /* Light Steel Blue */
   push_back_color_if_ok( bgc, QColor( 255, 255, 224 ) ); /* Light Yellow */
   push_back_color_if_ok( bgc, QColor( 0, 255, 0 ) ); /* Lime* */
   push_back_color_if_ok( bgc, QColor( 50, 205, 50 ) ); /* Lime Green */
   push_back_color_if_ok( bgc, QColor( 250, 240, 230 ) ); /* Linen */
   push_back_color_if_ok( bgc, QColor( 255, 0, 255 ) ); /* Magenta */
   push_back_color_if_ok( bgc, QColor( 128, 0, 0 ) ); /* Maroon* */
   push_back_color_if_ok( bgc, QColor( 102, 205, 170 ) ); /* Medium Aquamarine */
   push_back_color_if_ok( bgc, QColor( 0, 0, 205 ) ); /* Medium Blue */
   push_back_color_if_ok( bgc, QColor( 186, 85, 211 ) ); /* Medium Orchid */
   push_back_color_if_ok( bgc, QColor( 147, 112, 219 ) ); /* Medium Purple */
   push_back_color_if_ok( bgc, QColor( 60, 179, 113 ) ); /* Medium Sea Green */
   push_back_color_if_ok( bgc, QColor( 123, 104, 238 ) ); /* Medium Slate Blue */
   push_back_color_if_ok( bgc, QColor( 0, 250, 154 ) ); /* Medium Spring Green */
   push_back_color_if_ok( bgc, QColor( 72, 209, 204 ) ); /* Medium Turquoise */
   push_back_color_if_ok( bgc, QColor( 199, 21, 133 ) ); /* Medium Violet-Red */
   push_back_color_if_ok( bgc, QColor( 25, 25, 112 ) ); /* Midnight Blue */
   push_back_color_if_ok( bgc, QColor( 245, 255, 250 ) ); /* Mint Cream */
   push_back_color_if_ok( bgc, QColor( 255, 228, 225 ) ); /* Misty Rose */
   push_back_color_if_ok( bgc, QColor( 255, 228, 181 ) ); /* Moccasin */
   push_back_color_if_ok( bgc, QColor( 255, 222, 173 ) ); /* Navajo White */
   push_back_color_if_ok( bgc, QColor( 0, 0, 128 ) ); /* Navy* */
   push_back_color_if_ok( bgc, QColor( 253, 245, 230 ) ); /* Old Lace */
   push_back_color_if_ok( bgc, QColor( 128, 128, 0 ) ); /* Olive* */
   push_back_color_if_ok( bgc, QColor( 107, 142, 35 ) ); /* Olive Drab */
   push_back_color_if_ok( bgc, QColor( 255, 165, 0 ) ); /* Orange */
   push_back_color_if_ok( bgc, QColor( 255, 69, 0 ) ); /* Orange-Red */
   push_back_color_if_ok( bgc, QColor( 218, 112, 214 ) ); /* Orchid */
   push_back_color_if_ok( bgc, QColor( 238, 232, 170 ) ); /* Pale Goldenrod */
   push_back_color_if_ok( bgc, QColor( 152, 251, 152 ) ); /* Pale Green */
   push_back_color_if_ok( bgc, QColor( 175, 238, 238 ) ); /* Pale Turquoise */
   push_back_color_if_ok( bgc, QColor( 219, 112, 147 ) ); /* Pale Violet-Red */
   push_back_color_if_ok( bgc, QColor( 255, 239, 213 ) ); /* Papaya Whip */
   push_back_color_if_ok( bgc, QColor( 255, 218, 185 ) ); /* Peach Puff */
   push_back_color_if_ok( bgc, QColor( 205, 133, 63 ) ); /* Peru */
   push_back_color_if_ok( bgc, QColor( 255, 192, 203 ) ); /* Pink */
   push_back_color_if_ok( bgc, QColor( 221, 160, 221 ) ); /* Plum */
   push_back_color_if_ok( bgc, QColor( 176, 224, 230 ) ); /* Powder Blue */
   push_back_color_if_ok( bgc, QColor( 128, 0, 128 ) ); /* Purple* */
   push_back_color_if_ok( bgc, QColor( 255, 0, 0 ) ); /* Red* */
   push_back_color_if_ok( bgc, QColor( 188, 143, 143 ) ); /* Rosy Brown */
   push_back_color_if_ok( bgc, QColor( 65, 105, 225 ) ); /* Royal Blue */
   push_back_color_if_ok( bgc, QColor( 139, 69, 19 ) ); /* Saddle Brown */
   push_back_color_if_ok( bgc, QColor( 250, 128, 114 ) ); /* Salmon */
   push_back_color_if_ok( bgc, QColor( 244, 164, 96 ) ); /* Sandy Brown */
   push_back_color_if_ok( bgc, QColor( 46, 139, 87 ) ); /* Sea Green */
   push_back_color_if_ok( bgc, QColor( 255, 245, 238 ) ); /* Seashell */
   push_back_color_if_ok( bgc, QColor( 160, 82, 45 ) ); /* Sienna */
   push_back_color_if_ok( bgc, QColor( 192, 192, 192 ) ); /* Silver* */
   push_back_color_if_ok( bgc, QColor( 135, 206, 235 ) ); /* Sky Blue */
   push_back_color_if_ok( bgc, QColor( 106, 90, 205 ) ); /* Slate Blue */
   push_back_color_if_ok( bgc, QColor( 112, 128, 144 ) ); /* Slate Gray */
   push_back_color_if_ok( bgc, QColor( 255, 250, 250 ) ); /* Snow */
   push_back_color_if_ok( bgc, QColor( 0, 255, 127 ) ); /* Spring Green */
   push_back_color_if_ok( bgc, QColor( 70, 130, 180 ) ); /* Steel Blue */
   push_back_color_if_ok( bgc, QColor( 210, 180, 140 ) ); /* Tan */
   push_back_color_if_ok( bgc, QColor( 0, 128, 128 ) ); /* Teal* */
   push_back_color_if_ok( bgc, QColor( 216, 191, 216 ) ); /* Thistle */
   push_back_color_if_ok( bgc, QColor( 255, 99, 71 ) ); /* Tomato */
   push_back_color_if_ok( bgc, QColor( 64, 224, 208 ) ); /* Turquoise */
   push_back_color_if_ok( bgc, QColor( 238, 130, 238 ) ); /* Violet */
   push_back_color_if_ok( bgc, QColor( 208, 32, 144 ) ); /* Violet-Red */
   push_back_color_if_ok( bgc, QColor( 245, 222, 179 ) ); /* Wheat */
   push_back_color_if_ok( bgc, QColor( 255, 255, 255 ) ); /* White* */
   push_back_color_if_ok( bgc, QColor( 245, 245, 245 ) ); /* White Smoke */
   push_back_color_if_ok( bgc, QColor( 255, 255, 0 ) ); /* Yellow* */
   push_back_color_if_ok( bgc, QColor( 154, 205, 50 ) ); /* Yellow-Green */


   mQLabel * menu_icon = new mQLabel( "", this );
   {
      QPixmap * qpm = new QPixmap( 35, 30 );
      QPainter *pd  = new QPainter( qpm );
      pd->fillRect( 0, 0, 35, 30, QColor( 0,0,95 ) );
      pd->setPen( QPen( QColor( 220,210,210 ), 2 ) );
      pd->drawLine( 8, 10, 29, 10  );
      pd->drawLine( 8, 16, 29, 16 );
      pd->drawLine( 8, 22, 29, 22 );
      menu_icon->setPixmap( *qpm );
      menu_icon->pixmap_base = qpm;
   }
   {
      QPixmap * qpm = new QPixmap( 35, 30 );
      QPainter *pd  = new QPainter( qpm );
      pd->fillRect( 0, 0, 35, 30, QColor( 0,0,95 ) );
      pd->setPen( QPen( QColor( 200,128,0 ), 2 ) );
      pd->drawLine( 8, 10, 29, 10  );
      pd->drawLine( 8, 16, 29, 16 );
      pd->drawLine( 8, 22, 29, 22 );
      menu_icon->pixmap_hover = qpm;
   }

   connect( menu_icon, SIGNAL( pressed() ), SLOT( menu_pressed() ) );

   menu_button = new mQLabel( "", this );
   menu_button->setAlignment( Qt::AlignTop | Qt::AlignLeft );
   menu_button->mbuddy = menu_icon;
   connect( menu_button, SIGNAL( pressed() ), SLOT( menu_pressed() ) );

   gl_top->addWidget( menu_icon  , 0, 0 );
   gl_top->addWidget( menu_button, 0, 1 );
   gl_top->addWidget( title      , 0, 2 );
   gl_top->setColStretch( 0, 0 );
   gl_top->setColStretch( 1, 0 );
   gl_top->setColStretch( 2, 1 );

   QGridLayout * gl_menu = new QGridLayout( 0 );
   int gl_menu_pos = 0;
   {
      id_to_label[ "tools" ] = "Tools";
      id_to_icon [ "tools" ] = QPixmap( QImage( QString( "pngs/tools.png" ) ).smoothScale( 40, 60, QImage::ScaleMin ) );
      mQLabel* lbl = new mQLabel( "", this );
      lbl->setPixmap( id_to_icon[ "tools" ] );
      connect( lbl, SIGNAL( pressed() ), SLOT( tools_pressed() ) );

      mQLabel* lbl2 = new mQLabel( "<h4>Tools</h4>", this  );
      lbl2->setAlignment( Qt::AlignVCenter | Qt::AlignHCenter );
      connect( lbl2, SIGNAL( pressed() ), SLOT( tools_pressed() ) );

      lbl->mbuddy = lbl2;

      gl_menu->addWidget( lbl2, gl_menu_pos, 0 );
      gl_menu->addWidget( lbl , gl_menu_pos, 1 );
      gl_menu->setRowStretch( gl_menu_pos, 0 );
      gl_menu_pos++;
      menu_widgets.push_back( lbl );
      menu_widgets.push_back( lbl2 );
   }
   {
      id_to_label[ "build" ] = "Build";
      id_to_icon [ "build" ] = QPixmap( QImage( QString( "pngs/build.png" ) ).smoothScale( 40, 60, QImage::ScaleMin ) );
      mQLabel* lbl = new mQLabel( "", this );
      lbl->setPixmap( id_to_icon[ "build" ] );
      connect( lbl, SIGNAL( pressed() ), SLOT( build_pressed() ) );

      mQLabel* lbl2 = new mQLabel( "<h4>Build</h4>", this  );
      lbl2->setAlignment( Qt::AlignVCenter | Qt::AlignHCenter );
      connect( lbl2, SIGNAL( pressed() ), SLOT( build_pressed() ) );

      lbl->mbuddy = lbl2;

      gl_menu->addWidget( lbl2, gl_menu_pos, 0 );
      gl_menu->addWidget( lbl , gl_menu_pos, 1 );
      gl_menu->setRowStretch( gl_menu_pos, 0 );
      gl_menu_pos++;
      menu_widgets.push_back( lbl );
      menu_widgets.push_back( lbl2 );
   }
   {
      id_to_label[ "interact" ] = "Interact";
      id_to_icon [ "interact" ] = QPixmap( QImage( QString( "pngs/interact.png" ) ).smoothScale( 40, 60, QImage::ScaleMin ) );
      mQLabel* lbl = new mQLabel( "", this );
      lbl->setPixmap( id_to_icon[ "interact" ] );
      connect( lbl, SIGNAL( pressed() ), SLOT( interact_pressed() ) );

      mQLabel* lbl2 = new mQLabel( "<h4>Interact</h4>", this  );
      lbl2->setAlignment( Qt::AlignVCenter | Qt::AlignHCenter );
      connect( lbl2, SIGNAL( pressed() ), SLOT( interact_pressed() ) );

      lbl->mbuddy = lbl2;

      gl_menu->addWidget( lbl2, gl_menu_pos, 0 );
      gl_menu->addWidget( lbl , gl_menu_pos, 1 );
      gl_menu->setRowStretch( gl_menu_pos, 0 );
      gl_menu_pos++;
      menu_widgets.push_back( lbl );
      menu_widgets.push_back( lbl2 );
   }
   {
      id_to_label[ "simulate" ] = "Simulate";
      id_to_icon [ "simulate" ] = QPixmap( QImage( QString( "pngs/simulate.png" ) ).smoothScale( 40, 60, QImage::ScaleMin ) );
      mQLabel* lbl = new mQLabel( "", this );
      lbl->setPixmap( id_to_icon[ "simulate" ] );
      connect( lbl, SIGNAL( pressed() ), SLOT( simulate_pressed() ) );

      mQLabel* lbl2 = new mQLabel( "<h4>Simulate</h4>", this  );
      lbl2->setAlignment( Qt::AlignVCenter | Qt::AlignHCenter );
      connect( lbl2, SIGNAL( pressed() ), SLOT( simulate_pressed() ) );

      lbl->mbuddy = lbl2;

      gl_menu->addWidget( lbl2, gl_menu_pos, 0 );
      gl_menu->addWidget( lbl , gl_menu_pos, 1 );
      gl_menu->setRowStretch( gl_menu_pos, 0 );
      gl_menu_pos++;
      menu_widgets.push_back( lbl );
      menu_widgets.push_back( lbl2 );
   }
   {
      id_to_label[ "calculate" ] = "Calculate";
      id_to_icon [ "calculate" ] = QPixmap( QImage( QString( "pngs/calculate.png" ) ).smoothScale( 40, 60, QImage::ScaleMin ) );
      mQLabel* lbl = new mQLabel( "", this );
      lbl->setPixmap( id_to_icon[ "calculate" ] );
      connect( lbl, SIGNAL( pressed() ), SLOT( calculate_pressed() ) );

      mQLabel* lbl2 = new mQLabel( "<h4>Calculate</h4>", this  );
      lbl2->setAlignment( Qt::AlignVCenter | Qt::AlignHCenter );
      connect( lbl2, SIGNAL( pressed() ), SLOT( calculate_pressed() ) );

      lbl->mbuddy = lbl2;

      gl_menu->addWidget( lbl2, gl_menu_pos, 0 );
      gl_menu->addWidget( lbl , gl_menu_pos, 1 );
      gl_menu->setRowStretch( gl_menu_pos, 0 );
      gl_menu_pos++;
      menu_widgets.push_back( lbl );
      menu_widgets.push_back( lbl2 );
   }
   {
      id_to_label[ "analyze" ] = "Analyze";
      id_to_icon [ "analyze" ] = QPixmap( QImage( QString( "pngs/analyze.png" ) ).smoothScale( 40, 60, QImage::ScaleMin ) );
      mQLabel* lbl = new mQLabel( "", this );
      lbl->setPixmap( id_to_icon[ "analyze" ] );
      connect( lbl, SIGNAL( pressed() ), SLOT( analyze_pressed() ) );

      mQLabel* lbl2 = new mQLabel( "<h4>Analyze</h4>", this  );
      lbl2->setAlignment( Qt::AlignVCenter | Qt::AlignHCenter );
      connect( lbl2, SIGNAL( pressed() ), SLOT( analyze_pressed() ) );

      lbl->mbuddy = lbl2;

      gl_menu->addWidget( lbl2, gl_menu_pos, 0 );
      gl_menu->addWidget( lbl , gl_menu_pos, 1 );
      gl_menu->setRowStretch( gl_menu_pos, 0 );
      gl_menu_pos++;
      menu_widgets.push_back( lbl );
      menu_widgets.push_back( lbl2 );
   }

   {
      QLabel * lbl = new QLabel( "", this );
      gl_menu->addMultiCellWidget( lbl, gl_menu_pos, gl_menu_pos, 0, 1 );
      gl_menu->setRowStretch( gl_menu_pos, 1 );
   }

   gl_panel1 = new QGridLayout( 0, 1, 1, 5, 0 );
   gl_footer = new QGridLayout( 0 );
   {
      QLabel *lbl = new QLabel( this, "footer" );
      gl_footer->addWidget( lbl, 0, 0 );
   }

   QGridLayout * gl_mid = new QGridLayout( 0 );
   gl_mid->addLayout( gl_menu, 0, 0 );
   gl_mid->addLayout( gl_panel1, 0, 1 );
   gl_mid->setColStretch( 0, 0 );
   gl_mid->setColStretch( 1, 1 );

   QGridLayout * gl_main = new QGridLayout( this, 1, 1, 5, 0 );
   gl_main->addLayout( gl_top, 0, 0 );
   {
      QLabel * lbl = new QLabel( "", this );
      lbl->setMaximumHeight( 7 );
      gl_main->addWidget( lbl , 1, 0 );
   }
   gl_main->addLayout( gl_mid, 2, 0 );
   gl_main->addLayout( gl_footer, 3, 0 );
   gl_main->setRowStretch( 0, 0 );
   gl_main->setRowStretch( 1, 0 );
   gl_main->setRowStretch( 2, 0 );
   gl_main->setRowStretch( 3, 1 );
}

void sassie::menu_pressed()
{
   hide_widgets( menu_widgets, menu_widgets[ 0 ]->isVisible() );
}

void sassie::hide_widgets( QValueVector < QWidget * > &widgets, bool hide )
{
   for ( unsigned int i = 0; i < ( unsigned int ) widgets.size(); i++ )
   {
      hide ? widgets[ i ]->hide() : widgets[ i ]->show();
   }
}

void sassie::delete_widgets_layouts( QValueVector < QWidget * > &widgets, 
                                              QValueVector < QLayout * > &layouts
                                              )
{
   for ( unsigned int i = 0; i < ( unsigned int ) widgets.size(); i++ )
   {
      gl_panel1->remove( widgets[ i ] );
      delete widgets[ i ];
   }
   widgets.clear();
   for ( unsigned int i = 0; i < ( unsigned int ) layouts.size(); i++ )
   {
      gl_panel1->removeItem( layouts[ i ] );
      delete layouts[ i ];
   }
   layouts.clear();
}

void sassie::save_value( const QString & id, const QString & ext )
{
   if ( global_data_types.count( id ) )
   {
      QString did = id + ":" + ext;
      if ( global_data_types[ id ] == "le" )
      {
         global_data[ did ] = QVariant( ((QLineEdit *)panel1_widget_map[ id ])->text() );
         return;
      }
      if ( global_data_types[ id ] == "cb" )
      {
         global_data[ did ] = QVariant( ((QCheckBox *)panel1_widget_map[ id ])->isChecked() );
         return;
      }
      if ( global_data_types[ id ] == "rb" )
      {
         global_data[ did ] = QVariant( ((QRadioButton *)panel1_widget_map[ id ])->isChecked() );
         return;
      }
      if ( global_data_types[ id ] == "lb" )
      {
         global_data[ did ] = QVariant( ((QListBox *)panel1_widget_map[ id ])->currentItem() );
         return;
      }
      if ( global_data_types[ id ] == "lbl" )
      {
         global_data[ did ] = QVariant( ((QLabel *)panel1_widget_map[ id ])->text() );
         return;
      }
      if ( global_data_types[ id ] == "te" )
      {
         global_data[ did ] = QVariant( ((QTextEdit *)panel1_widget_map[ id ])->text() );
         return;
      }
      if ( global_data_types[ id ] == "file" )
      {
         global_data[ did ] = ((mQPushButton *)panel1_widget_map[ id ])->data;
         return;
      }
      if ( global_data_types[ id ] == "files" )
      {
         global_data[ did ] = ((mQPushButton *)panel1_widget_map[ id ])->data;
         return;
      }
      qDebug(
         QString( "Error: sassie::save_value( %1 ): unsupporded data type '%2'\n" )
         .arg( id )
         .arg( global_data_types[ id ] )
             )
         ;
      return;
   }
   qDebug(
      QString( "Error: sassie::save_value( %1, %2 ): no global_data_type[] found\n" )
      .arg( id )
      .arg( ext )
          )
      ;
}

void sassie::save_last_value( const QString & id )
{
   save_value( id, "last_value" );
}

void sassie::save_default_value( const QString & id )
{
   if ( !global_data.count( id ) )
   {
      save_value( id, "default_value" );
   }
}

void sassie::save_last_values()
{
   for ( int i = 0; i < (int) panel1_inputs.size(); ++i )
   {
      save_last_value( panel1_inputs[ i ] );
   }
}

void sassie::reset_value( const QString & id, const QString & ext )
{
   if ( global_data_types.count( id ) )
   {
      QString did = id + ":" + ext;
      if ( ext == "last_value" && !global_data.count( did ) )
      {
         QString did2 = id + ":default_value";
         if ( global_data.count( did2 ) )
         {
            global_data[ did ] = global_data[ did2 ];
         }
      }

      if ( global_data.count( did ) )
      {
         if ( global_data_types[ id ] == "le" )
         {
            ((QLineEdit *)panel1_widget_map[ id ])->setText( global_data[ did ].toString() );
            save_last_value( id );
            return;
         } 
         if ( global_data_types[ id ] == "cb" )
         {
            ((QCheckBox *)panel1_widget_map[ id ])->setChecked( global_data[ did ].toBool() );
            save_last_value( id );
            return;
         } 
         if ( global_data_types[ id ] == "rb" )
         {
            ((QRadioButton *)panel1_widget_map[ id ])->setChecked( global_data[ did ].toBool() );
            save_last_value( id );
            return;
         } 
         if ( global_data_types[ id ] == "lb" )
         {
            ((QListBox *)panel1_widget_map[ id ])->setSelected( global_data[ did ].toInt(), true );
            save_last_value( id );
            return;
         } 
         if ( global_data_types[ id ] == "lbl" )
         {
            ((QLabel *)panel1_widget_map[ id ])->setText( global_data[ did ].toString() );
            save_last_value( id );
            return;
         } 
         if ( global_data_types[ id ] == "te" )
         {
            ((QTextEdit *)panel1_widget_map[ id ])->setText( global_data[ did ].toString() );
            save_last_value( id );
            return;
         } 
         if ( global_data_types[ id ] == "plt" )
         {
            QwtPlot * plt = (QwtPlot *)panel1_widget_map[ id ];
            plt->clear();
            // add the curves processed from global_data[ did ].toString()
            QValueVector < plotData > * plot_results = 
               UTILITY_JSON::array_string_to_qvv( global_data[ did ].toString() );
            for ( int i = 0; i < (int) plot_results->size(); ++i )
            {
               QString label = QString( "%1" ).arg( i ); // todo: generate labels
               long Iq = plt->insertCurve( label );
               plt->setCurveStyle( Iq, QwtCurve::Lines );
               
               plt->setCurveData( Iq, 
                                  (double *)&( (*plot_results)[ i ].x[ 0 ] ),
                                  (double *)&( (*plot_results)[ i ].y[ 0 ] ),
                                  (*plot_results)[ i ].x.size()
                                  );
               plt->setCurvePen( Iq, QPen( plot_colors[ i ], 1, SolidLine));
            }
            plt->replot();
            delete plot_results;
            if ( ext == "default_value" )
            {
               global_data[ id + ":last_value" ] = global_data[ did ];
            }
            return;
         } 
         if ( global_data_types[ id ] == "file" )
         {
            const QStringList * qsl = & global_data[ did ].asStringList();
            ((mQPushButton *)panel1_widget_map[ id ])->data = global_data[ did ];
            ((mQPushButton *)panel1_widget_map[ id ])->mbuddy->setText( qsl->size() ?
                                                                        QFileInfo( (*qsl)[ 0 ] ).fileName() :
                                                                        QString( "No file selected." ) );
            if ( ext == "default_value" )
            {
               save_last_value( id );
            }
            return;
         } 
         if ( global_data_types[ id ] == "files" )
         {
            const QStringList * qsl = & global_data[ did ].asStringList();
            ((mQPushButton *)panel1_widget_map[ id ])->data = global_data[ did ];
            int count = qsl->size();
            ((mQPushButton *)panel1_widget_map[ id ])->mbuddy->setText( count ?
                                                                        ( count == 1 ?
                                                                          QFileInfo( (*qsl)[ 0 ] ).fileName() :
                                                                          QString( "%1 files selected" ).arg( count ) ) : QString( "No files selected." ) );
            if ( ext == "default_value" )
            {
               save_last_value( id );
            }
            return;
         } 
         qDebug(
                QString( "Error: sassie::reset_value( %1, %2 ): unsupporded data type '%3'\n" )
                .arg( id )
                .arg( ext )
                .arg( global_data_types[ id ] )
                )
            ;
      } else {
         qDebug( QString( "Error: sassie::reset value( %1, %2 ) no global data found\n" ).arg( id ).arg( ext) );
      }
      return;
   }
   qDebug(
      QString( "Error: sassie::reset_value( %1, %2 ): no global_data_type[] found\n" )
      .arg( id )
      .arg( ext )
          )
      ;
}

void sassie::reset_output_values( const QString & ext )
{
   for ( int i = 0; i < (int) panel1_outputs.size(); ++i )
   {
      reset_value( panel1_outputs[ i ], ext );
   }
}

void sassie::reset_values( const QString & ext )
{
   for ( int i = 0; i < (int) panel1_inputs.size(); ++i )
   {
      reset_value( panel1_inputs[ i ], ext );
   }
   reset_output_values( ext );
}

void sassie::reset_default_values()
{
   reset_values( "default_value" );
}

void sassie::reset_last_values()
{
   reset_values( "last_value" );
}

QString sassie::get_last_value( const QString & id, bool & skip )
{
   skip = false;
   if ( global_data_types.count( id ) )
   {
      QString did = id + ":last_value";
      if ( global_data_types[ id ] == "le" || 
           global_data_types[ id ] == "lbl" ||
           global_data_types[ id ] == "te" )
      {
         return global_data[ did ].asString();
      }
      if ( global_data_types[ id ] == "cb" )
      {
         skip = !global_data[ did ].asBool();
         return skip ? "off" : "on";
      }
      if ( global_data_types[ id ] == "rb" )
      {
         skip = !global_data[ did ].asBool();
         return skip ? QString( "" ) : ((mQRadioButton *)panel1_widget_map[ id ])->data.asString();
      }

      if ( global_data_types[ id ] == "lb" )
      {
         return panel1_map_input[ "lb:" + ((QListBox *)panel1_widget_map[ id ])->text( global_data[ did ].asInt() ) ];
      }
      if ( global_data_types[ id ] == "file" ||
           global_data_types[ id ] == "files" )
      {
         return global_data[ did ].asStringList().size() ?
            "[\"" + global_data[ did ].asStringList().join( "\",\"" ) + "\"]" :
            "[ ]";
      }
      qDebug(
         QString( "Error: sassie::get_last_value( %1 ): unsupporded data type '%2'\n" )
         .arg( id )
         .arg( global_data_types[ id ] )
             )
         ;
      return "**error**";
   }
   qDebug(
      QString( "Error: sassie::get_last_value( %1 ): no global_data_type[] found\n" )
      .arg( id )
          )
      ;
   return "**error**";
}

QString sassie::input_to_json( const QString & mod )
{
   QMap < QString, QString > inputs;
   QRegExp rxclean = QRegExp( "^" + mod + ":" );
   bool skip;
   for ( int i = 0; i < (int) panel1_inputs.size(); ++i )
   {
      QString cleaned = panel1_inputs[ i ];
      if ( panel1_map_input.count( cleaned ) )
      {
         qDebug( QString( "input_to_json::renaming %1 %2" ).arg( cleaned ).arg( panel1_map_input[ cleaned ] ) );
         cleaned = panel1_map_input[ cleaned ];
      }
      cleaned.replace( rxclean, "" );
      QString last_value = get_last_value( panel1_inputs[ i ], skip );
      if ( !skip )
      {
         inputs[ cleaned ] = last_value;
      }
   }
   return UTILITY_JSON::compose( inputs );
}

void sassie::process_results( const QString & mod )
{
   //   qDebug( process_json[ mod ] );
   QMap < QString, QString > results = UTILITY_JSON::split( process_json[ mod ] );
   // now store in global data and display if active   

   QString errors;
   for ( QMap < QString, QString >::iterator it = results.begin();
         it != results.end();
         it++ )
   {
      qDebug( QString( "%1 => %2" ).arg( it.key() ).arg( it.data().left( 100 ) ) );
      QString key = mod + ":" + it.key();
      if ( !global_data.count( key + ":default_value" ) )
      {
         qDebug( "unknown key received: " + key );
         if ( !errors.length() )
         {
            errors = "Unexpected results:\n";
         }
         errors += QString( "  %1 => %2\n" ).arg( it.key() ).arg( it.data().left( 100 ) );
      } else {
         if ( global_data_types.count( key ) )
         {
            bool ok = false;
            if ( global_data_types[ key ] == "le" ||
                 global_data_types[ key ] == "lbl" )
            {
               global_data[ key + ":last_value" ] = QVariant( it.data() );
               ok = true;
            }
            if ( global_data_types[ key ] == "cb" ||
                 global_data_types[ key ] == "rb" )
            {
               global_data[ key + ":last_value" ] = QVariant( true );
               ok = true;
            }
            if ( !ok )
            {
               qDebug( "sassie::process_results() unsupported output data type: " + global_data_types[ key ] );
            }          
            //         global_data[ key + ":last_value" ] = QVariant( it.data() );
            if ( current_module_id == mod &&
                 global_data_types.count( key ) )
            {
               reset_value( key, "last_value" );
            } else {
               qDebug( "current module wrong or no global data types for key: " + key );
            }
         } else {
            qDebug( "sassie::process_results() global_data_types[] does not contain: " + key );
         }            
      }
   }

   {
      QString key = mod + ":_errorMessages";    
      global_data[ key + ":last_value" ] = QVariant( errors );
      if ( current_module_id == mod &&
           global_data_types.count( key ) )
      {
         reset_value( key, "last_value" );
      } else {
         qDebug( "current module wrong or no global data types for key: " + key );
      }
   }
}

void sassie::push_back_color_if_ok( QColor bg, QColor set )
{
   double sum = 
      fabs( (float) bg.red  () - (float) set.red  () ) +
      fabs( (float) bg.green() - (float) set.green() ) +
      fabs( (float) bg.blue () - (float) set.blue () );
   if ( sum > 150 )
   {
      if ( plot_colors.size() )
      {
         bg = plot_colors.back();
         double sum = 
            fabs( (float) bg.red  () - (float) set.red  () ) +
            fabs( (float) bg.green() - (float) set.green() ) +
            fabs( (float) bg.blue () - (float) set.blue () );
         if ( sum > 100 )
         {
            plot_colors.push_back( set );
         }
      } else {
         plot_colors.push_back( set );
      }
   }
}

void sassie::browse_filenames( const QString & label,
                                        const QString & id,
                                        bool  multiple_files )
{
   QStringList filenames;
   QString tag = "Select for " + label;

   mQPushButton * pb = (mQPushButton *)panel1_widget_map[ id ];

   if ( multiple_files )
   {
      filenames = QFileDialog::getOpenFileNames( QString::null,
                                                 QString::null,
                                                 this,
                                                 tag,
                                                 tag
                                                 );
   } else {
      QString filename = QFileDialog::getOpenFileName( QString::null,
                                                       QString::null,
                                                       this,
                                                       tag,
                                                       tag
                                                       );
      if ( !filename.isEmpty() )
      {
         filenames << filename;
      }
   }
   pb->data = QVariant( filenames );
   save_value( id, "tmp_value" );
   reset_value( id, "tmp_value" );
}

int main( int argc, char *argv[] )
{
   QApplication *app = new QApplication( argc, argv );
   app->setPalette( QPalette( QColor( 128,170,150 ), QColor( 0,0,95 ) ) );
   palette_app = app->palette();
   palette_app.setColor( QColorGroup::ButtonText, QColor( 0, 0, 0 ) );
   app->setPalette( palette_app );
   sassie * sassie_main = new sassie();
   app->setMainWidget( sassie_main );
   sassie_main->show();
   return app->exec();
}
void sassie::browse_simulate_1_input4()
{
   browse_filenames( "Input file", "simulate_1:input4" );
}
void sassie::browse_simulate_1_input5()
{
   browse_filenames( "Input files", "simulate_1:input5" ,true );
}
#include "tools.h"
#include "build.h"
#include "interact.h"
#include "simulate.h"
#include "calculate.h"
#include "analyze.h"
#include "tools_center.h"
#include "tools_align.h"
#include "tools_data_interpolation.h"
#include "build_build_1.h"
#include "build_build_2.h"
#include "interact_interact_1.h"
#include "interact_interact_2.h"
#include "simulate_simulate_1.h"
#include "simulate_simulate_2.h"
#include "calculate_calculate_1.h"
#include "calculate_calculate_2.h"
#include "analyze_analyze_1.h"
#include "analyze_analyze_2.h"
