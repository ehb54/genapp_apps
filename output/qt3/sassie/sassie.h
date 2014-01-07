#ifndef DEFINED_sassie
#define DEFINED_sassie

#include <qapplication.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qpixmap.h>
#include <qimage.h>
#include <qmessagebox.h>
#include <qmap.h>
#include <qvaluevector.h>
#include <qpainter.h>
#include <qpushbutton.h>
#include <qlineedit.h>
#include <qvalidator.h>
#include <qcheckbox.h>
#include <qregexp.h>
#include <qvariant.h>
#include <qprocess.h>
#include <qwt_plot.h>
#include <qcursor.h>
#include <qfileinfo.h>
#include <qfiledialog.h>
#include <qstringlist.h>
#include <qradiobutton.h>
#include <qbuttongroup.h>
#include <qlistbox.h>
#include <qtextedit.h>
#include "utility_routines.h"

class mQLabel : public QLabel
{
   Q_OBJECT

   public:

      mQLabel ( QWidget *parent = 0 , const char * name = 0 );
      mQLabel ( const QString & text, QWidget *parent = 0 , const char * name = 0 );
      ~mQLabel();

      mQLabel * mbuddy;
      QPixmap * pixmap_base;
      QPixmap * pixmap_hover;

   signals:
      void pressed();

   protected:
      virtual void mousePressEvent ( QMouseEvent *e );
      virtual void enterEvent      ( QEvent *e );
      virtual void leaveEvent      ( QEvent *e );
};


class mQPushButton : public QPushButton
{
   Q_OBJECT

   public:

      mQPushButton  ( QWidget *parent = 0 , const char * name = 0 );
      ~mQPushButton ();
      QLabel *      mbuddy;
      QVariant      data;

   protected:
      virtual void enterEvent      ( QEvent *e );
      virtual void leaveEvent      ( QEvent *e );
};

class mQRadioButton : public QRadioButton
{
   Q_OBJECT

   public:

      mQRadioButton  ( QWidget *parent = 0 , const char * name = 0 );
      ~mQRadioButton ();

      QVariant      data;
};

class sassie : public QFrame
{
   Q_OBJECT

   public:

      sassie();

   private:

      QMap < QString, QPixmap >     id_to_icon;
      QMap < QString, QString >     id_to_label;
      QValueVector < QWidget * >    menu_widgets;
      void                          hide_widgets( QValueVector < QWidget * > &widgets, bool hide = true );
      QValueVector < QWidget * >    panel1_widgets;
      QValueVector < QLayout * >    panel1_layouts;
      QValueVector < QWidget * >    panel1_sub_widgets;
      QValueVector < QLayout * >    panel1_sub_layouts;
      void                          delete_widgets_layouts( QValueVector < QWidget * > &widgets,
                                                            QValueVector < QLayout * > &layouts );

      mQLabel *                     menu_button;
      QGridLayout *                 gl_panel1;
      QGridLayout *                 gl_footer;

      QPalette *                    palette_le;
      QPalette *                    palette_cb;
      QPalette *                    palette_lbl_error;
      QPalette *                    palette_plot;

      QMap < QString, QVariant >    global_data;
      QMap < QString, QString >     global_data_types;

      QValueVector < QString >      panel1_inputs;
      QValueVector < QString >      panel1_outputs;
      QMap < QString, QWidget * >   panel1_widget_map;
      QMap < QString, QString >     panel1_map_input;

      void                          save_value          ( const QString & id, const QString & ext );
      void                          save_last_value     ( const QString & id );
      void                          save_default_value  ( const QString & id );
      void                          save_last_values    ();

      void                          reset_value         ( const QString & id, const QString & ext );
      void                          reset_values        ( const QString & ext );
      void                          reset_output_values ( const QString & ext );
      void                          reset_default_values();
      void                          reset_last_values   ();

      QString                       get_last_value      ( const QString & id , bool & skip );
      QString                       input_to_json       ( const QString & mod );

      QString                       current_module_id;
      QMap < QString, QString >     process_json;

      void                          process_results     ( const QString & mod );
      QValueVector < QColor >       plot_colors;
      void                          push_back_color_if_ok( QColor bg, QColor set );

      void                          browse_filenames( const QString & label,
                                                      const QString & id,
                                                      bool  multiple_files = false );

   private slots:

      void                          menu_pressed();

   private slots:
      void                          tools_pressed();

   private slots:
      void                          build_pressed();

   private slots:
      void                          interact_pressed();

   private slots:
      void                          simulate_pressed();

   private slots:
      void                          calculate_pressed();

   private slots:
      void                          analyze_pressed();

   private slots:
      void                          module_load_tools_center();
      void                          module_submit_tools_center();
      void                          module_reset_tools_center();
      void                          readyReadStdout_tools_center();
      void                          readyReadStderr_tools_center();
      void                          launchFinished_tools_center();
      void                          processExited_tools_center();

   private:
      QProcess *                    process_tools_center;

   private slots:
      void                          module_load_tools_align();
      void                          module_submit_tools_align();
      void                          module_reset_tools_align();
      void                          readyReadStdout_tools_align();
      void                          readyReadStderr_tools_align();
      void                          launchFinished_tools_align();
      void                          processExited_tools_align();

   private:
      QProcess *                    process_tools_align;

   private slots:
      void                          module_load_tools_data_interpolation();
      void                          module_submit_tools_data_interpolation();
      void                          module_reset_tools_data_interpolation();
      void                          readyReadStdout_tools_data_interpolation();
      void                          readyReadStderr_tools_data_interpolation();
      void                          launchFinished_tools_data_interpolation();
      void                          processExited_tools_data_interpolation();

   private:
      QProcess *                    process_tools_data_interpolation;

   private slots:
      void                          module_load_build_build_1();
      void                          module_submit_build_build_1();
      void                          module_reset_build_build_1();
      void                          readyReadStdout_build_build_1();
      void                          readyReadStderr_build_build_1();
      void                          launchFinished_build_build_1();
      void                          processExited_build_build_1();

   private:
      QProcess *                    process_build_build_1;

   private slots:
      void                          module_load_build_build_2();
      void                          module_submit_build_build_2();
      void                          module_reset_build_build_2();
      void                          readyReadStdout_build_build_2();
      void                          readyReadStderr_build_build_2();
      void                          launchFinished_build_build_2();
      void                          processExited_build_build_2();

   private:
      QProcess *                    process_build_build_2;

   private slots:
      void                          module_load_interact_interact_1();
      void                          module_submit_interact_interact_1();
      void                          module_reset_interact_interact_1();
      void                          readyReadStdout_interact_interact_1();
      void                          readyReadStderr_interact_interact_1();
      void                          launchFinished_interact_interact_1();
      void                          processExited_interact_interact_1();

   private:
      QProcess *                    process_interact_interact_1;

   private slots:
      void                          module_load_interact_interact_2();
      void                          module_submit_interact_interact_2();
      void                          module_reset_interact_interact_2();
      void                          readyReadStdout_interact_interact_2();
      void                          readyReadStderr_interact_interact_2();
      void                          launchFinished_interact_interact_2();
      void                          processExited_interact_interact_2();

   private:
      QProcess *                    process_interact_interact_2;

   private slots:
      void                          module_load_simulate_simulate_1();
      void                          module_submit_simulate_simulate_1();
      void                          module_reset_simulate_simulate_1();
      void                          readyReadStdout_simulate_simulate_1();
      void                          readyReadStderr_simulate_simulate_1();
      void                          launchFinished_simulate_simulate_1();
      void                          processExited_simulate_simulate_1();

   private:
      QProcess *                    process_simulate_simulate_1;

   private slots:
      void                          module_load_simulate_simulate_2();
      void                          module_submit_simulate_simulate_2();
      void                          module_reset_simulate_simulate_2();
      void                          readyReadStdout_simulate_simulate_2();
      void                          readyReadStderr_simulate_simulate_2();
      void                          launchFinished_simulate_simulate_2();
      void                          processExited_simulate_simulate_2();

   private:
      QProcess *                    process_simulate_simulate_2;

   private slots:
      void                          module_load_calculate_calculate_1();
      void                          module_submit_calculate_calculate_1();
      void                          module_reset_calculate_calculate_1();
      void                          readyReadStdout_calculate_calculate_1();
      void                          readyReadStderr_calculate_calculate_1();
      void                          launchFinished_calculate_calculate_1();
      void                          processExited_calculate_calculate_1();

   private:
      QProcess *                    process_calculate_calculate_1;

   private slots:
      void                          module_load_calculate_calculate_2();
      void                          module_submit_calculate_calculate_2();
      void                          module_reset_calculate_calculate_2();
      void                          readyReadStdout_calculate_calculate_2();
      void                          readyReadStderr_calculate_calculate_2();
      void                          launchFinished_calculate_calculate_2();
      void                          processExited_calculate_calculate_2();

   private:
      QProcess *                    process_calculate_calculate_2;

   private slots:
      void                          module_load_analyze_analyze_1();
      void                          module_submit_analyze_analyze_1();
      void                          module_reset_analyze_analyze_1();
      void                          readyReadStdout_analyze_analyze_1();
      void                          readyReadStderr_analyze_analyze_1();
      void                          launchFinished_analyze_analyze_1();
      void                          processExited_analyze_analyze_1();

   private:
      QProcess *                    process_analyze_analyze_1;

   private slots:
      void                          module_load_analyze_analyze_2();
      void                          module_submit_analyze_analyze_2();
      void                          module_reset_analyze_analyze_2();
      void                          readyReadStdout_analyze_analyze_2();
      void                          readyReadStderr_analyze_analyze_2();
      void                          launchFinished_analyze_analyze_2();
      void                          processExited_analyze_analyze_2();

   private:
      QProcess *                    process_analyze_analyze_2;

   private slots:
      void                          browse_simulate_1_input4();

   private slots:
      void                          browse_simulate_1_input5();
};

#endif // DEFINED_sassie
