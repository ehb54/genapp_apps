void sassie::module_load_tools_data_interpolation()
{
   delete_widgets_layouts( panel1_sub_widgets, panel1_sub_layouts );
   panel1_widget_map.clear();
   panel1_inputs.clear();
   panel1_outputs.clear();
   panel1_map_input.clear();

   current_module_id = "data_interpolation";

   {
      QLabel * lbl = new QLabel( "", this );
      lbl->setMaximumHeight( 12 );
      lbl->show();
      gl_panel1->addWidget( lbl, 1, 0 );
      panel1_sub_widgets.push_back( lbl );

   }

   QGridLayout * gl = new QGridLayout( 0, 1, 1, 3, 3 );
   {
      QLabel * lbl = new QLabel( "", this );
      lbl->setMaximumHeight( 12 );
      lbl->show();
      gl->addWidget( lbl, 0, 2 );
      panel1_sub_widgets.push_back( lbl );
   }

   int ppos = 0; // the base position in the gl_sub_panel

   {
      QLabel * lbl = new QLabel( "Start x  ", this );
      lbl->setMaximumHeight( 22 );
      lbl->show();
      gl->addWidget( lbl, ppos, 0 );
      panel1_sub_widgets.push_back( lbl );

      QLineEdit * le = new QLineEdit( this );
      le->setMaximumHeight( 22 );
      le->setPalette( *palette_le );
      le->setValidator( new QDoubleValidator( le ) );
      ((QDoubleValidator*)le->validator())->setDecimals( 8 );
      le->setText( QString( "%1" ).arg( 0 ) ); 
     
     
      le->show();
      gl->addWidget( le, ppos, 1 );
      panel1_sub_widgets.push_back( le );

      QString id = "data_interpolation:startx";
      global_data_types[ id ] = "le";
      panel1_widget_map[ id ] = le;
      panel1_inputs.push_back( id );
      save_default_value( id );

      ppos++;
   }
   {
      QLabel * lbl = new QLabel( "End x  ", this );
      lbl->setMaximumHeight( 22 );
      lbl->show();
      gl->addWidget( lbl, ppos, 0 );
      panel1_sub_widgets.push_back( lbl );

      QLineEdit * le = new QLineEdit( this );
      le->setMaximumHeight( 22 );
      le->setPalette( *palette_le );
      le->setValidator( new QDoubleValidator( le ) );
      ((QDoubleValidator*)le->validator())->setDecimals( 8 );
      le->setText( QString( "%1" ).arg( 26 ) ); 
     
     
      le->show();
      gl->addWidget( le, ppos, 1 );
      panel1_sub_widgets.push_back( le );

      QString id = "data_interpolation:endx";
      global_data_types[ id ] = "le";
      panel1_widget_map[ id ] = le;
      panel1_inputs.push_back( id );
      save_default_value( id );

      ppos++;
   }
   {
      QLabel * lbl = new QLabel( "Delta x  ", this );
      lbl->setMaximumHeight( 22 );
      lbl->show();
      gl->addWidget( lbl, ppos, 0 );
      panel1_sub_widgets.push_back( lbl );

      QLineEdit * le = new QLineEdit( this );
      le->setMaximumHeight( 22 );
      le->setPalette( *palette_le );
      le->setValidator( new QDoubleValidator( le ) );
      ((QDoubleValidator*)le->validator())->setDecimals( 8 );
      le->setText( QString( "%1" ).arg( 0.1 ) ); 
     
     
      le->show();
      gl->addWidget( le, ppos, 1 );
      panel1_sub_widgets.push_back( le );

      QString id = "data_interpolation:deltax";
      global_data_types[ id ] = "le";
      panel1_widget_map[ id ] = le;
      panel1_inputs.push_back( id );
      save_default_value( id );

      ppos++;
   }
   {
      mQLabel * lbl = new mQLabel( "abs  ", this );
      lbl->setMaximumHeight( 22 );
      lbl->show();
      gl->addWidget( lbl, ppos, 0 );
      panel1_sub_widgets.push_back( lbl );

      QCheckBox * cb = new QCheckBox( this );
      cb->setMaximumHeight( 22 );
      cb->setPalette( *palette_cb );
     
      cb->show();
      gl->addWidget( cb, ppos, 1 );
      panel1_sub_widgets.push_back( cb );
      connect( lbl, SIGNAL( pressed() ), cb, SLOT( toggle() ) );

      QString id = "data_interpolation:abs";
      global_data_types[ id ] = "cb";
      panel1_widget_map[ id ] = cb;
      panel1_inputs.push_back( id );
      save_default_value( id );

      ppos++;
   }
   QHBoxLayout * hbl = new QHBoxLayout( 0 );
   {
      mQPushButton * pb = new mQPushButton( this );
      pb->setText( "Submit" );
      pb->setMaximumHeight( 22 );
      pb->show();
      connect( pb, SIGNAL( clicked() ), SLOT( module_submit_tools_data_interpolation() ) );
      hbl->addWidget( pb );
      hbl->addSpacing( 3 );
      panel1_sub_widgets.push_back( pb );
   }
   {
      mQPushButton * pb = new mQPushButton( this );
      pb->setText( "Reset to default values" );
      pb->setMaximumHeight( 22 );
      pb->show();
      connect( pb, SIGNAL( clicked() ), SLOT(module_reset_tools_data_interpolation() ) );
      hbl->addWidget( pb );
      panel1_sub_widgets.push_back( pb );
   }
   gl->addMultiCellLayout( hbl, ppos, ppos, 0, 1 );
   panel1_sub_layouts.push_back( hbl );
   ppos++;

   {
      QLabel * lbl = new QLabel( "", this );
      lbl->setPalette( *palette_lbl_error );
      lbl->show();
      gl->addMultiCellWidget( lbl, ppos, ppos, 0, 1 );
      panel1_sub_widgets.push_back( lbl );

      QString id = "data_interpolation:_errorMessages";
      global_data_types[ id ] = "lbl";
      panel1_widget_map[ id ] = lbl;
      panel1_outputs.push_back( id );
      save_default_value( id );
   }
   ppos++;
   {
      QLabel * lbl = new QLabel( "output integer  ", this );
      lbl->setMaximumHeight( 22 );
      lbl->show();
      gl->addWidget( lbl, ppos, 0 );
      panel1_sub_widgets.push_back( lbl );

      QLineEdit * le = new QLineEdit( this );
      le->setMaximumHeight( 22 );
      le->setPalette( *palette_le );
      le->setReadOnly( true );
      le->show();
      gl->addWidget( le, ppos, 1 );
      panel1_sub_widgets.push_back( le );

      QString id = "data_interpolation:output1";
      global_data_types[ id ] = "le";
      panel1_widget_map[ id ] = le;
      panel1_outputs.push_back( id );
      save_default_value( id );

      ppos++;
   }
   {
      QLabel * lbl = new QLabel( "output float  ", this );
      lbl->setMaximumHeight( 22 );
      lbl->show();
      gl->addWidget( lbl, ppos, 0 );
      panel1_sub_widgets.push_back( lbl );

      QLineEdit * le = new QLineEdit( this );
      le->setMaximumHeight( 22 );
      le->setPalette( *palette_le );
      le->setReadOnly( true );
      le->show();
      gl->addWidget( le, ppos, 1 );
      panel1_sub_widgets.push_back( le );

      QString id = "data_interpolation:output2";
      global_data_types[ id ] = "le";
      panel1_widget_map[ id ] = le;
      panel1_outputs.push_back( id );
      save_default_value( id );

      ppos++;
   }
   {
      QLabel * lbl = new QLabel( "output text  ", this );
      lbl->setMaximumHeight( 22 );
      lbl->show();
      gl->addWidget( lbl, ppos, 0 );
      panel1_sub_widgets.push_back( lbl );

      QLineEdit * le = new QLineEdit( this );
      le->setMaximumHeight( 22 );
      le->setPalette( *palette_le );
      le->setReadOnly( true );
      le->show();
      gl->addWidget( le, ppos, 1 );
      panel1_sub_widgets.push_back( le );

      QString id = "data_interpolation:output3";
      global_data_types[ id ] = "le";
      panel1_widget_map[ id ] = le;
      panel1_outputs.push_back( id );
      save_default_value( id );

      ppos++;
   }
   {
      QLabel * lbl = new QLabel( "plot 2d  ", this );
      lbl->setMaximumHeight( 22 );
      lbl->show();
      gl->addWidget( lbl, ppos, 0 );
      panel1_sub_widgets.push_back( lbl );

      ppos++;
      QwtPlot * plt  = new QwtPlot( this );
      plt->setPalette( *palette_plot );
      plt->setGridMajPen(QPen( Qt::lightGray, 1, DotLine));
      plt->setGridMinPen(QPen( Qt::gray, 1, DotLine));
      plt->show();
      gl->addMultiCellWidget( plt, ppos, ppos, 0, 2 );
      panel1_sub_widgets.push_back( plt );

      QString id = "data_interpolation:plotout4";
      global_data_types[ id ] = "plt";
      panel1_widget_map[ id ] = plt;
      panel1_outputs.push_back( id );
      //      save_default_value( id );
      global_data[ id + ":default_value" ] = QVariant( "[[]]" );

      ppos++;
   }
   {
      QLabel * lbl = new QLabel( "output 5  ", this );
      lbl->setMaximumHeight( 22 );
      lbl->show();
      gl->addWidget( lbl, ppos, 0 );
      panel1_sub_widgets.push_back( lbl );

      QCheckBox * cb = new QCheckBox( this );
      cb->setMaximumHeight( 22 );
      cb->setPalette( *palette_cb );
      cb->setEnabled( false );
      cb->show();
      gl->addWidget( cb, ppos, 1 );
      panel1_sub_widgets.push_back( cb );

      QString id = "data_interpolation:output5";
      global_data_types[ id ] = "cb";
      panel1_widget_map[ id ] = cb;
      panel1_outputs.push_back( id );
      save_default_value( id );

      ppos++;
   }
   {
      QLabel * lbl = new QLabel( "output again  ", this );
      lbl->setMaximumHeight( 22 );
      lbl->show();
      gl->addWidget( lbl, ppos, 0 );
      panel1_sub_widgets.push_back( lbl );

      QCheckBox * cb = new QCheckBox( this );
      cb->setMaximumHeight( 22 );
      cb->setPalette( *palette_cb );
      cb->setEnabled( false );
      cb->show();
      gl->addWidget( cb, ppos, 1 );
      panel1_sub_widgets.push_back( cb );

      QString id = "data_interpolation:output6";
      global_data_types[ id ] = "cb";
      panel1_widget_map[ id ] = cb;
      panel1_outputs.push_back( id );
      save_default_value( id );

      ppos++;
   }

   reset_last_values();

   gl->setColStretch( 0, 0 );
   gl->setColStretch( 1, 0 );
   gl->setColStretch( 2, 1 );
   gl_panel1->addMultiCellLayout( gl, 2, 2, 0, 2 );
   panel1_sub_layouts.push_back( gl );
}

void sassie::module_reset_tools_data_interpolation()
{
   reset_default_values();
}

void sassie::module_submit_tools_data_interpolation()
{
   save_last_values();
   reset_output_values( "default_value" );
   // qDebug( input_to_json( "data_interpolation" ) );
   // we should check if process already running

   QString program = "/src/sassie2/bin/data_interpolation";

   QFileInfo qfi( program );
   if ( !qfi.exists() || !qfi.isExecutable() || qfi.isDir() )
   {
      QString id = "data_interpolation";
      QString key = id + ":_errorMessages";    
      global_data[ key + ":last_value" ] = QVariant( "Unexpected results:\n   error => command not found or not executable" );
      if ( current_module_id == id &&
           global_data_types.count( key ) )
      {
         reset_value( key, "last_value" );
      }
      return;
   }
   //   qDebug( "process exists and isExecutable" );
   
   process_tools_data_interpolation = new QProcess( this );
   process_tools_data_interpolation->addArgument( program );
   process_tools_data_interpolation->addArgument( input_to_json( "data_interpolation" ) );
   connect( process_tools_data_interpolation, 
            SIGNAL( readyReadStdout() ), 
            this, 
            SLOT( readyReadStdout_tools_data_interpolation() ) );
   connect( process_tools_data_interpolation, 
            SIGNAL( readyReadStderr() ), 
            this, 
            SLOT( readyReadStderr_tools_data_interpolation() ) );
   connect( process_tools_data_interpolation, 
            SIGNAL( launchFinished() ), 
            this, 
            SLOT( launchFinished_tools_data_interpolation() ) );
   connect( process_tools_data_interpolation, 
            SIGNAL( processExited() ), 
            this, 
            SLOT( processExited_tools_data_interpolation() ) );
   if ( !process_tools_data_interpolation->start() )
   { 
      qDebug( "error starting process" );
      return;
   }
   process_json[ "data_interpolation" ] = "";
   qDebug( "process started" );
}

void sassie::readyReadStdout_tools_data_interpolation()
{
   while ( process_tools_data_interpolation->canReadLineStdout() )
   {
      process_json[ "data_interpolation" ] += process_tools_data_interpolation->readLineStdout();
   }
}

void sassie::readyReadStderr_tools_data_interpolation()
{
   while ( process_tools_data_interpolation->canReadLineStderr() )
   {
      qDebug( process_tools_data_interpolation->readLineStderr() );
   }
}

void sassie::launchFinished_tools_data_interpolation()
{
   // qDebug( "process_tools_data_interpolation->launchFinished()" );
   disconnect( process_tools_data_interpolation,
               SIGNAL( launchFinished_tools_data_interpolation() ),
               0, 0 );
}

void sassie::processExited_tools_data_interpolation()
{
   qDebug( "process_tools_data_interpolation->processExited()" );
   readyReadStdout_tools_data_interpolation();
   readyReadStderr_tools_data_interpolation();

   disconnect( process_tools_data_interpolation, 0, 0, 0 );
   // post process data into output fields, add unexpected data etc
   // delete process_tools_data_interpolation;
   // process_tools_data_interpolation = (QProcess *) 0;
   process_results( "data_interpolation" );
}
