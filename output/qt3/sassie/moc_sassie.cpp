/****************************************************************************
** mQLabel meta object code from reading C++ file 'sassie.h'
**
** Created: Mon Jan 6 23:06:45 2014
**      by: The Qt MOC ($Id: qt/moc_yacc.cpp   3.3.8   edited Feb 2 14:59 $)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#undef QT_NO_COMPAT
#include "sassie.h"
#include <qmetaobject.h>
#include <qapplication.h>

#include <private/qucomextra_p.h>
#if !defined(Q_MOC_OUTPUT_REVISION) || (Q_MOC_OUTPUT_REVISION != 26)
#error "This file was generated using the moc from 3.3.8b. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

const char *mQLabel::className() const
{
    return "mQLabel";
}

QMetaObject *mQLabel::metaObj = 0;
static QMetaObjectCleanUp cleanUp_mQLabel( "mQLabel", &mQLabel::staticMetaObject );

#ifndef QT_NO_TRANSLATION
QString mQLabel::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "mQLabel", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString mQLabel::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "mQLabel", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* mQLabel::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = QLabel::staticMetaObject();
    static const QUMethod signal_0 = {"pressed", 0, 0 };
    static const QMetaData signal_tbl[] = {
	{ "pressed()", &signal_0, QMetaData::Public }
    };
    metaObj = QMetaObject::new_metaobject(
	"mQLabel", parentObject,
	0, 0,
	signal_tbl, 1,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_mQLabel.setMetaObject( metaObj );
    return metaObj;
}

void* mQLabel::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "mQLabel" ) )
	return this;
    return QLabel::qt_cast( clname );
}

// SIGNAL pressed
void mQLabel::pressed()
{
    activate_signal( staticMetaObject()->signalOffset() + 0 );
}

bool mQLabel::qt_invoke( int _id, QUObject* _o )
{
    return QLabel::qt_invoke(_id,_o);
}

bool mQLabel::qt_emit( int _id, QUObject* _o )
{
    switch ( _id - staticMetaObject()->signalOffset() ) {
    case 0: pressed(); break;
    default:
	return QLabel::qt_emit(_id,_o);
    }
    return TRUE;
}
#ifndef QT_NO_PROPERTIES

bool mQLabel::qt_property( int id, int f, QVariant* v)
{
    return QLabel::qt_property( id, f, v);
}

bool mQLabel::qt_static_property( QObject* , int , int , QVariant* ){ return FALSE; }
#endif // QT_NO_PROPERTIES


const char *mQPushButton::className() const
{
    return "mQPushButton";
}

QMetaObject *mQPushButton::metaObj = 0;
static QMetaObjectCleanUp cleanUp_mQPushButton( "mQPushButton", &mQPushButton::staticMetaObject );

#ifndef QT_NO_TRANSLATION
QString mQPushButton::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "mQPushButton", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString mQPushButton::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "mQPushButton", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* mQPushButton::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = QPushButton::staticMetaObject();
    metaObj = QMetaObject::new_metaobject(
	"mQPushButton", parentObject,
	0, 0,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_mQPushButton.setMetaObject( metaObj );
    return metaObj;
}

void* mQPushButton::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "mQPushButton" ) )
	return this;
    return QPushButton::qt_cast( clname );
}

bool mQPushButton::qt_invoke( int _id, QUObject* _o )
{
    return QPushButton::qt_invoke(_id,_o);
}

bool mQPushButton::qt_emit( int _id, QUObject* _o )
{
    return QPushButton::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool mQPushButton::qt_property( int id, int f, QVariant* v)
{
    return QPushButton::qt_property( id, f, v);
}

bool mQPushButton::qt_static_property( QObject* , int , int , QVariant* ){ return FALSE; }
#endif // QT_NO_PROPERTIES


const char *mQRadioButton::className() const
{
    return "mQRadioButton";
}

QMetaObject *mQRadioButton::metaObj = 0;
static QMetaObjectCleanUp cleanUp_mQRadioButton( "mQRadioButton", &mQRadioButton::staticMetaObject );

#ifndef QT_NO_TRANSLATION
QString mQRadioButton::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "mQRadioButton", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString mQRadioButton::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "mQRadioButton", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* mQRadioButton::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = QRadioButton::staticMetaObject();
    metaObj = QMetaObject::new_metaobject(
	"mQRadioButton", parentObject,
	0, 0,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_mQRadioButton.setMetaObject( metaObj );
    return metaObj;
}

void* mQRadioButton::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "mQRadioButton" ) )
	return this;
    return QRadioButton::qt_cast( clname );
}

bool mQRadioButton::qt_invoke( int _id, QUObject* _o )
{
    return QRadioButton::qt_invoke(_id,_o);
}

bool mQRadioButton::qt_emit( int _id, QUObject* _o )
{
    return QRadioButton::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool mQRadioButton::qt_property( int id, int f, QVariant* v)
{
    return QRadioButton::qt_property( id, f, v);
}

bool mQRadioButton::qt_static_property( QObject* , int , int , QVariant* ){ return FALSE; }
#endif // QT_NO_PROPERTIES


const char *sassie::className() const
{
    return "sassie";
}

QMetaObject *sassie::metaObj = 0;
static QMetaObjectCleanUp cleanUp_sassie( "sassie", &sassie::staticMetaObject );

#ifndef QT_NO_TRANSLATION
QString sassie::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "sassie", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString sassie::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "sassie", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* sassie::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = QFrame::staticMetaObject();
    static const QUMethod slot_0 = {"menu_pressed", 0, 0 };
    static const QUMethod slot_1 = {"tools_pressed", 0, 0 };
    static const QUMethod slot_2 = {"build_pressed", 0, 0 };
    static const QUMethod slot_3 = {"interact_pressed", 0, 0 };
    static const QUMethod slot_4 = {"simulate_pressed", 0, 0 };
    static const QUMethod slot_5 = {"calculate_pressed", 0, 0 };
    static const QUMethod slot_6 = {"analyze_pressed", 0, 0 };
    static const QUMethod slot_7 = {"module_load_tools_center", 0, 0 };
    static const QUMethod slot_8 = {"module_submit_tools_center", 0, 0 };
    static const QUMethod slot_9 = {"module_reset_tools_center", 0, 0 };
    static const QUMethod slot_10 = {"readyReadStdout_tools_center", 0, 0 };
    static const QUMethod slot_11 = {"readyReadStderr_tools_center", 0, 0 };
    static const QUMethod slot_12 = {"launchFinished_tools_center", 0, 0 };
    static const QUMethod slot_13 = {"processExited_tools_center", 0, 0 };
    static const QUMethod slot_14 = {"module_load_tools_align", 0, 0 };
    static const QUMethod slot_15 = {"module_submit_tools_align", 0, 0 };
    static const QUMethod slot_16 = {"module_reset_tools_align", 0, 0 };
    static const QUMethod slot_17 = {"readyReadStdout_tools_align", 0, 0 };
    static const QUMethod slot_18 = {"readyReadStderr_tools_align", 0, 0 };
    static const QUMethod slot_19 = {"launchFinished_tools_align", 0, 0 };
    static const QUMethod slot_20 = {"processExited_tools_align", 0, 0 };
    static const QUMethod slot_21 = {"module_load_tools_data_interpolation", 0, 0 };
    static const QUMethod slot_22 = {"module_submit_tools_data_interpolation", 0, 0 };
    static const QUMethod slot_23 = {"module_reset_tools_data_interpolation", 0, 0 };
    static const QUMethod slot_24 = {"readyReadStdout_tools_data_interpolation", 0, 0 };
    static const QUMethod slot_25 = {"readyReadStderr_tools_data_interpolation", 0, 0 };
    static const QUMethod slot_26 = {"launchFinished_tools_data_interpolation", 0, 0 };
    static const QUMethod slot_27 = {"processExited_tools_data_interpolation", 0, 0 };
    static const QUMethod slot_28 = {"module_load_build_build_1", 0, 0 };
    static const QUMethod slot_29 = {"module_submit_build_build_1", 0, 0 };
    static const QUMethod slot_30 = {"module_reset_build_build_1", 0, 0 };
    static const QUMethod slot_31 = {"readyReadStdout_build_build_1", 0, 0 };
    static const QUMethod slot_32 = {"readyReadStderr_build_build_1", 0, 0 };
    static const QUMethod slot_33 = {"launchFinished_build_build_1", 0, 0 };
    static const QUMethod slot_34 = {"processExited_build_build_1", 0, 0 };
    static const QUMethod slot_35 = {"module_load_build_build_2", 0, 0 };
    static const QUMethod slot_36 = {"module_submit_build_build_2", 0, 0 };
    static const QUMethod slot_37 = {"module_reset_build_build_2", 0, 0 };
    static const QUMethod slot_38 = {"readyReadStdout_build_build_2", 0, 0 };
    static const QUMethod slot_39 = {"readyReadStderr_build_build_2", 0, 0 };
    static const QUMethod slot_40 = {"launchFinished_build_build_2", 0, 0 };
    static const QUMethod slot_41 = {"processExited_build_build_2", 0, 0 };
    static const QUMethod slot_42 = {"module_load_interact_interact_1", 0, 0 };
    static const QUMethod slot_43 = {"module_submit_interact_interact_1", 0, 0 };
    static const QUMethod slot_44 = {"module_reset_interact_interact_1", 0, 0 };
    static const QUMethod slot_45 = {"readyReadStdout_interact_interact_1", 0, 0 };
    static const QUMethod slot_46 = {"readyReadStderr_interact_interact_1", 0, 0 };
    static const QUMethod slot_47 = {"launchFinished_interact_interact_1", 0, 0 };
    static const QUMethod slot_48 = {"processExited_interact_interact_1", 0, 0 };
    static const QUMethod slot_49 = {"module_load_interact_interact_2", 0, 0 };
    static const QUMethod slot_50 = {"module_submit_interact_interact_2", 0, 0 };
    static const QUMethod slot_51 = {"module_reset_interact_interact_2", 0, 0 };
    static const QUMethod slot_52 = {"readyReadStdout_interact_interact_2", 0, 0 };
    static const QUMethod slot_53 = {"readyReadStderr_interact_interact_2", 0, 0 };
    static const QUMethod slot_54 = {"launchFinished_interact_interact_2", 0, 0 };
    static const QUMethod slot_55 = {"processExited_interact_interact_2", 0, 0 };
    static const QUMethod slot_56 = {"module_load_simulate_simulate_1", 0, 0 };
    static const QUMethod slot_57 = {"module_submit_simulate_simulate_1", 0, 0 };
    static const QUMethod slot_58 = {"module_reset_simulate_simulate_1", 0, 0 };
    static const QUMethod slot_59 = {"readyReadStdout_simulate_simulate_1", 0, 0 };
    static const QUMethod slot_60 = {"readyReadStderr_simulate_simulate_1", 0, 0 };
    static const QUMethod slot_61 = {"launchFinished_simulate_simulate_1", 0, 0 };
    static const QUMethod slot_62 = {"processExited_simulate_simulate_1", 0, 0 };
    static const QUMethod slot_63 = {"module_load_simulate_simulate_2", 0, 0 };
    static const QUMethod slot_64 = {"module_submit_simulate_simulate_2", 0, 0 };
    static const QUMethod slot_65 = {"module_reset_simulate_simulate_2", 0, 0 };
    static const QUMethod slot_66 = {"readyReadStdout_simulate_simulate_2", 0, 0 };
    static const QUMethod slot_67 = {"readyReadStderr_simulate_simulate_2", 0, 0 };
    static const QUMethod slot_68 = {"launchFinished_simulate_simulate_2", 0, 0 };
    static const QUMethod slot_69 = {"processExited_simulate_simulate_2", 0, 0 };
    static const QUMethod slot_70 = {"module_load_calculate_calculate_1", 0, 0 };
    static const QUMethod slot_71 = {"module_submit_calculate_calculate_1", 0, 0 };
    static const QUMethod slot_72 = {"module_reset_calculate_calculate_1", 0, 0 };
    static const QUMethod slot_73 = {"readyReadStdout_calculate_calculate_1", 0, 0 };
    static const QUMethod slot_74 = {"readyReadStderr_calculate_calculate_1", 0, 0 };
    static const QUMethod slot_75 = {"launchFinished_calculate_calculate_1", 0, 0 };
    static const QUMethod slot_76 = {"processExited_calculate_calculate_1", 0, 0 };
    static const QUMethod slot_77 = {"module_load_calculate_calculate_2", 0, 0 };
    static const QUMethod slot_78 = {"module_submit_calculate_calculate_2", 0, 0 };
    static const QUMethod slot_79 = {"module_reset_calculate_calculate_2", 0, 0 };
    static const QUMethod slot_80 = {"readyReadStdout_calculate_calculate_2", 0, 0 };
    static const QUMethod slot_81 = {"readyReadStderr_calculate_calculate_2", 0, 0 };
    static const QUMethod slot_82 = {"launchFinished_calculate_calculate_2", 0, 0 };
    static const QUMethod slot_83 = {"processExited_calculate_calculate_2", 0, 0 };
    static const QUMethod slot_84 = {"module_load_analyze_analyze_1", 0, 0 };
    static const QUMethod slot_85 = {"module_submit_analyze_analyze_1", 0, 0 };
    static const QUMethod slot_86 = {"module_reset_analyze_analyze_1", 0, 0 };
    static const QUMethod slot_87 = {"readyReadStdout_analyze_analyze_1", 0, 0 };
    static const QUMethod slot_88 = {"readyReadStderr_analyze_analyze_1", 0, 0 };
    static const QUMethod slot_89 = {"launchFinished_analyze_analyze_1", 0, 0 };
    static const QUMethod slot_90 = {"processExited_analyze_analyze_1", 0, 0 };
    static const QUMethod slot_91 = {"module_load_analyze_analyze_2", 0, 0 };
    static const QUMethod slot_92 = {"module_submit_analyze_analyze_2", 0, 0 };
    static const QUMethod slot_93 = {"module_reset_analyze_analyze_2", 0, 0 };
    static const QUMethod slot_94 = {"readyReadStdout_analyze_analyze_2", 0, 0 };
    static const QUMethod slot_95 = {"readyReadStderr_analyze_analyze_2", 0, 0 };
    static const QUMethod slot_96 = {"launchFinished_analyze_analyze_2", 0, 0 };
    static const QUMethod slot_97 = {"processExited_analyze_analyze_2", 0, 0 };
    static const QUMethod slot_98 = {"browse_simulate_1_input4", 0, 0 };
    static const QUMethod slot_99 = {"browse_simulate_1_input5", 0, 0 };
    static const QMetaData slot_tbl[] = {
	{ "menu_pressed()", &slot_0, QMetaData::Private },
	{ "tools_pressed()", &slot_1, QMetaData::Private },
	{ "build_pressed()", &slot_2, QMetaData::Private },
	{ "interact_pressed()", &slot_3, QMetaData::Private },
	{ "simulate_pressed()", &slot_4, QMetaData::Private },
	{ "calculate_pressed()", &slot_5, QMetaData::Private },
	{ "analyze_pressed()", &slot_6, QMetaData::Private },
	{ "module_load_tools_center()", &slot_7, QMetaData::Private },
	{ "module_submit_tools_center()", &slot_8, QMetaData::Private },
	{ "module_reset_tools_center()", &slot_9, QMetaData::Private },
	{ "readyReadStdout_tools_center()", &slot_10, QMetaData::Private },
	{ "readyReadStderr_tools_center()", &slot_11, QMetaData::Private },
	{ "launchFinished_tools_center()", &slot_12, QMetaData::Private },
	{ "processExited_tools_center()", &slot_13, QMetaData::Private },
	{ "module_load_tools_align()", &slot_14, QMetaData::Private },
	{ "module_submit_tools_align()", &slot_15, QMetaData::Private },
	{ "module_reset_tools_align()", &slot_16, QMetaData::Private },
	{ "readyReadStdout_tools_align()", &slot_17, QMetaData::Private },
	{ "readyReadStderr_tools_align()", &slot_18, QMetaData::Private },
	{ "launchFinished_tools_align()", &slot_19, QMetaData::Private },
	{ "processExited_tools_align()", &slot_20, QMetaData::Private },
	{ "module_load_tools_data_interpolation()", &slot_21, QMetaData::Private },
	{ "module_submit_tools_data_interpolation()", &slot_22, QMetaData::Private },
	{ "module_reset_tools_data_interpolation()", &slot_23, QMetaData::Private },
	{ "readyReadStdout_tools_data_interpolation()", &slot_24, QMetaData::Private },
	{ "readyReadStderr_tools_data_interpolation()", &slot_25, QMetaData::Private },
	{ "launchFinished_tools_data_interpolation()", &slot_26, QMetaData::Private },
	{ "processExited_tools_data_interpolation()", &slot_27, QMetaData::Private },
	{ "module_load_build_build_1()", &slot_28, QMetaData::Private },
	{ "module_submit_build_build_1()", &slot_29, QMetaData::Private },
	{ "module_reset_build_build_1()", &slot_30, QMetaData::Private },
	{ "readyReadStdout_build_build_1()", &slot_31, QMetaData::Private },
	{ "readyReadStderr_build_build_1()", &slot_32, QMetaData::Private },
	{ "launchFinished_build_build_1()", &slot_33, QMetaData::Private },
	{ "processExited_build_build_1()", &slot_34, QMetaData::Private },
	{ "module_load_build_build_2()", &slot_35, QMetaData::Private },
	{ "module_submit_build_build_2()", &slot_36, QMetaData::Private },
	{ "module_reset_build_build_2()", &slot_37, QMetaData::Private },
	{ "readyReadStdout_build_build_2()", &slot_38, QMetaData::Private },
	{ "readyReadStderr_build_build_2()", &slot_39, QMetaData::Private },
	{ "launchFinished_build_build_2()", &slot_40, QMetaData::Private },
	{ "processExited_build_build_2()", &slot_41, QMetaData::Private },
	{ "module_load_interact_interact_1()", &slot_42, QMetaData::Private },
	{ "module_submit_interact_interact_1()", &slot_43, QMetaData::Private },
	{ "module_reset_interact_interact_1()", &slot_44, QMetaData::Private },
	{ "readyReadStdout_interact_interact_1()", &slot_45, QMetaData::Private },
	{ "readyReadStderr_interact_interact_1()", &slot_46, QMetaData::Private },
	{ "launchFinished_interact_interact_1()", &slot_47, QMetaData::Private },
	{ "processExited_interact_interact_1()", &slot_48, QMetaData::Private },
	{ "module_load_interact_interact_2()", &slot_49, QMetaData::Private },
	{ "module_submit_interact_interact_2()", &slot_50, QMetaData::Private },
	{ "module_reset_interact_interact_2()", &slot_51, QMetaData::Private },
	{ "readyReadStdout_interact_interact_2()", &slot_52, QMetaData::Private },
	{ "readyReadStderr_interact_interact_2()", &slot_53, QMetaData::Private },
	{ "launchFinished_interact_interact_2()", &slot_54, QMetaData::Private },
	{ "processExited_interact_interact_2()", &slot_55, QMetaData::Private },
	{ "module_load_simulate_simulate_1()", &slot_56, QMetaData::Private },
	{ "module_submit_simulate_simulate_1()", &slot_57, QMetaData::Private },
	{ "module_reset_simulate_simulate_1()", &slot_58, QMetaData::Private },
	{ "readyReadStdout_simulate_simulate_1()", &slot_59, QMetaData::Private },
	{ "readyReadStderr_simulate_simulate_1()", &slot_60, QMetaData::Private },
	{ "launchFinished_simulate_simulate_1()", &slot_61, QMetaData::Private },
	{ "processExited_simulate_simulate_1()", &slot_62, QMetaData::Private },
	{ "module_load_simulate_simulate_2()", &slot_63, QMetaData::Private },
	{ "module_submit_simulate_simulate_2()", &slot_64, QMetaData::Private },
	{ "module_reset_simulate_simulate_2()", &slot_65, QMetaData::Private },
	{ "readyReadStdout_simulate_simulate_2()", &slot_66, QMetaData::Private },
	{ "readyReadStderr_simulate_simulate_2()", &slot_67, QMetaData::Private },
	{ "launchFinished_simulate_simulate_2()", &slot_68, QMetaData::Private },
	{ "processExited_simulate_simulate_2()", &slot_69, QMetaData::Private },
	{ "module_load_calculate_calculate_1()", &slot_70, QMetaData::Private },
	{ "module_submit_calculate_calculate_1()", &slot_71, QMetaData::Private },
	{ "module_reset_calculate_calculate_1()", &slot_72, QMetaData::Private },
	{ "readyReadStdout_calculate_calculate_1()", &slot_73, QMetaData::Private },
	{ "readyReadStderr_calculate_calculate_1()", &slot_74, QMetaData::Private },
	{ "launchFinished_calculate_calculate_1()", &slot_75, QMetaData::Private },
	{ "processExited_calculate_calculate_1()", &slot_76, QMetaData::Private },
	{ "module_load_calculate_calculate_2()", &slot_77, QMetaData::Private },
	{ "module_submit_calculate_calculate_2()", &slot_78, QMetaData::Private },
	{ "module_reset_calculate_calculate_2()", &slot_79, QMetaData::Private },
	{ "readyReadStdout_calculate_calculate_2()", &slot_80, QMetaData::Private },
	{ "readyReadStderr_calculate_calculate_2()", &slot_81, QMetaData::Private },
	{ "launchFinished_calculate_calculate_2()", &slot_82, QMetaData::Private },
	{ "processExited_calculate_calculate_2()", &slot_83, QMetaData::Private },
	{ "module_load_analyze_analyze_1()", &slot_84, QMetaData::Private },
	{ "module_submit_analyze_analyze_1()", &slot_85, QMetaData::Private },
	{ "module_reset_analyze_analyze_1()", &slot_86, QMetaData::Private },
	{ "readyReadStdout_analyze_analyze_1()", &slot_87, QMetaData::Private },
	{ "readyReadStderr_analyze_analyze_1()", &slot_88, QMetaData::Private },
	{ "launchFinished_analyze_analyze_1()", &slot_89, QMetaData::Private },
	{ "processExited_analyze_analyze_1()", &slot_90, QMetaData::Private },
	{ "module_load_analyze_analyze_2()", &slot_91, QMetaData::Private },
	{ "module_submit_analyze_analyze_2()", &slot_92, QMetaData::Private },
	{ "module_reset_analyze_analyze_2()", &slot_93, QMetaData::Private },
	{ "readyReadStdout_analyze_analyze_2()", &slot_94, QMetaData::Private },
	{ "readyReadStderr_analyze_analyze_2()", &slot_95, QMetaData::Private },
	{ "launchFinished_analyze_analyze_2()", &slot_96, QMetaData::Private },
	{ "processExited_analyze_analyze_2()", &slot_97, QMetaData::Private },
	{ "browse_simulate_1_input4()", &slot_98, QMetaData::Private },
	{ "browse_simulate_1_input5()", &slot_99, QMetaData::Private }
    };
    metaObj = QMetaObject::new_metaobject(
	"sassie", parentObject,
	slot_tbl, 100,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_sassie.setMetaObject( metaObj );
    return metaObj;
}

void* sassie::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "sassie" ) )
	return this;
    return QFrame::qt_cast( clname );
}

bool sassie::qt_invoke( int _id, QUObject* _o )
{
    switch ( _id - staticMetaObject()->slotOffset() ) {
    case 0: menu_pressed(); break;
    case 1: tools_pressed(); break;
    case 2: build_pressed(); break;
    case 3: interact_pressed(); break;
    case 4: simulate_pressed(); break;
    case 5: calculate_pressed(); break;
    case 6: analyze_pressed(); break;
    case 7: module_load_tools_center(); break;
    case 8: module_submit_tools_center(); break;
    case 9: module_reset_tools_center(); break;
    case 10: readyReadStdout_tools_center(); break;
    case 11: readyReadStderr_tools_center(); break;
    case 12: launchFinished_tools_center(); break;
    case 13: processExited_tools_center(); break;
    case 14: module_load_tools_align(); break;
    case 15: module_submit_tools_align(); break;
    case 16: module_reset_tools_align(); break;
    case 17: readyReadStdout_tools_align(); break;
    case 18: readyReadStderr_tools_align(); break;
    case 19: launchFinished_tools_align(); break;
    case 20: processExited_tools_align(); break;
    case 21: module_load_tools_data_interpolation(); break;
    case 22: module_submit_tools_data_interpolation(); break;
    case 23: module_reset_tools_data_interpolation(); break;
    case 24: readyReadStdout_tools_data_interpolation(); break;
    case 25: readyReadStderr_tools_data_interpolation(); break;
    case 26: launchFinished_tools_data_interpolation(); break;
    case 27: processExited_tools_data_interpolation(); break;
    case 28: module_load_build_build_1(); break;
    case 29: module_submit_build_build_1(); break;
    case 30: module_reset_build_build_1(); break;
    case 31: readyReadStdout_build_build_1(); break;
    case 32: readyReadStderr_build_build_1(); break;
    case 33: launchFinished_build_build_1(); break;
    case 34: processExited_build_build_1(); break;
    case 35: module_load_build_build_2(); break;
    case 36: module_submit_build_build_2(); break;
    case 37: module_reset_build_build_2(); break;
    case 38: readyReadStdout_build_build_2(); break;
    case 39: readyReadStderr_build_build_2(); break;
    case 40: launchFinished_build_build_2(); break;
    case 41: processExited_build_build_2(); break;
    case 42: module_load_interact_interact_1(); break;
    case 43: module_submit_interact_interact_1(); break;
    case 44: module_reset_interact_interact_1(); break;
    case 45: readyReadStdout_interact_interact_1(); break;
    case 46: readyReadStderr_interact_interact_1(); break;
    case 47: launchFinished_interact_interact_1(); break;
    case 48: processExited_interact_interact_1(); break;
    case 49: module_load_interact_interact_2(); break;
    case 50: module_submit_interact_interact_2(); break;
    case 51: module_reset_interact_interact_2(); break;
    case 52: readyReadStdout_interact_interact_2(); break;
    case 53: readyReadStderr_interact_interact_2(); break;
    case 54: launchFinished_interact_interact_2(); break;
    case 55: processExited_interact_interact_2(); break;
    case 56: module_load_simulate_simulate_1(); break;
    case 57: module_submit_simulate_simulate_1(); break;
    case 58: module_reset_simulate_simulate_1(); break;
    case 59: readyReadStdout_simulate_simulate_1(); break;
    case 60: readyReadStderr_simulate_simulate_1(); break;
    case 61: launchFinished_simulate_simulate_1(); break;
    case 62: processExited_simulate_simulate_1(); break;
    case 63: module_load_simulate_simulate_2(); break;
    case 64: module_submit_simulate_simulate_2(); break;
    case 65: module_reset_simulate_simulate_2(); break;
    case 66: readyReadStdout_simulate_simulate_2(); break;
    case 67: readyReadStderr_simulate_simulate_2(); break;
    case 68: launchFinished_simulate_simulate_2(); break;
    case 69: processExited_simulate_simulate_2(); break;
    case 70: module_load_calculate_calculate_1(); break;
    case 71: module_submit_calculate_calculate_1(); break;
    case 72: module_reset_calculate_calculate_1(); break;
    case 73: readyReadStdout_calculate_calculate_1(); break;
    case 74: readyReadStderr_calculate_calculate_1(); break;
    case 75: launchFinished_calculate_calculate_1(); break;
    case 76: processExited_calculate_calculate_1(); break;
    case 77: module_load_calculate_calculate_2(); break;
    case 78: module_submit_calculate_calculate_2(); break;
    case 79: module_reset_calculate_calculate_2(); break;
    case 80: readyReadStdout_calculate_calculate_2(); break;
    case 81: readyReadStderr_calculate_calculate_2(); break;
    case 82: launchFinished_calculate_calculate_2(); break;
    case 83: processExited_calculate_calculate_2(); break;
    case 84: module_load_analyze_analyze_1(); break;
    case 85: module_submit_analyze_analyze_1(); break;
    case 86: module_reset_analyze_analyze_1(); break;
    case 87: readyReadStdout_analyze_analyze_1(); break;
    case 88: readyReadStderr_analyze_analyze_1(); break;
    case 89: launchFinished_analyze_analyze_1(); break;
    case 90: processExited_analyze_analyze_1(); break;
    case 91: module_load_analyze_analyze_2(); break;
    case 92: module_submit_analyze_analyze_2(); break;
    case 93: module_reset_analyze_analyze_2(); break;
    case 94: readyReadStdout_analyze_analyze_2(); break;
    case 95: readyReadStderr_analyze_analyze_2(); break;
    case 96: launchFinished_analyze_analyze_2(); break;
    case 97: processExited_analyze_analyze_2(); break;
    case 98: browse_simulate_1_input4(); break;
    case 99: browse_simulate_1_input5(); break;
    default:
	return QFrame::qt_invoke( _id, _o );
    }
    return TRUE;
}

bool sassie::qt_emit( int _id, QUObject* _o )
{
    return QFrame::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool sassie::qt_property( int id, int f, QVariant* v)
{
    return QFrame::qt_property( id, f, v);
}

bool sassie::qt_static_property( QObject* , int , int , QVariant* ){ return FALSE; }
#endif // QT_NO_PROPERTIES
