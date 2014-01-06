#ifndef sassie_UTILITY_ROUTINES_H
#define sassie_UTILITY_ROUTINES_H

#include <qstring.h>
#include <qregexp.h>
#include <qmap.h>
#include <qvaluevector.h>

using namespace std;

struct plotData {
   QValueVector < double > x;
   QValueVector < double > y;
};

class UTILITY_JSON
{
 public:
   static QMap < QString, QString > split( QString );
   static QString compose( QMap < QString, QString > & );
   static QValueVector < plotData > * array_string_to_qvv( const QString & s );
};

#endif
