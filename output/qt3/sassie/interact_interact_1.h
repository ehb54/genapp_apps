void sassie::module_load_interact_interact_1()
{
   delete_widgets_layouts( panel1_sub_widgets, panel1_sub_layouts );
   panel1_widget_map.clear();
   panel1_inputs.clear();
   panel1_outputs.clear();
   panel1_map_input.clear();

   current_module_id = "interact_1";

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
      QLabel * lbl = new QLabel( "Input value 1  ", this );
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
      ((QDoubleValidator*)le->validator())->setBottom( 0 ); 
      ((QDoubleValidator*)le->validator())->setTop( 1e+50 ); 
      le->show();
      gl->addWidget( le, ppos, 1 );
      panel1_sub_widgets.push_back( le );

      QString id = "interact_1:input1";
      global_data_types[ id ] = "le";
      panel1_widget_map[ id ] = le;
      panel1_inputs.push_back( id );
      save_default_value( id );

      ppos++;
   }
   {
      QLabel * lbl = new QLabel( "Input value 2  ", this );
      lbl->setMaximumHeight( 22 );
      lbl->show();
      gl->addWidget( lbl, ppos, 0 );
      panel1_sub_widgets.push_back( lbl );

      QLineEdit * le = new QLineEdit( this );
      le->setMaximumHeight( 22 );
      le->setPalette( *palette_le );
      le->setValidator( new QIntValidator( le ) );
      le->setText( QString( "%1" ).arg( 1 ) ); 
      ((QIntValidator*)le->validator())->setBottom( -10 ); 
      ((QIntValidator*)le->validator())->setTop( 10 ); 
      le->show();
      gl->addWidget( le, ppos, 1 );
      panel1_sub_widgets.push_back( le );

      QString id = "interact_1:input2";
      global_data_types[ id ] = "le";
      panel1_widget_map[ id ] = le;
      panel1_inputs.push_back( id );
      save_default_value( id );

      ppos++;
   }
   {
      mQLabel * lbl = new mQLabel( "Input value 3  ", this );
      lbl->setMaximumHeight( 22 );
      lbl->show();
      gl->addWidget( lbl, ppos, 0 );
      panel1_sub_widgets.push_back( lbl );

      QCheckBox * cb = new QCheckBox( this );
      cb->setMaximumHeight( 22 );
      cb->setPalette( *palette_cb );
      cb->setChecked( true ); 
      cb->show();
      gl->addWidget( cb, ppos, 1 );
      panel1_sub_widgets.push_back( cb );
      connect( lbl, SIGNAL( pressed() ), cb, SLOT( toggle() ) );

      QString id = "interact_1:input3";
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
      connect( pb, SIGNAL( clicked() ), SLOT( module_submit_interact_interact_1() ) );
      hbl->addWidget( pb );
      hbl->addSpacing( 3 );
      panel1_sub_widgets.push_back( pb );
   }
   {
      mQPushButton * pb = new mQPushButton( this );
      pb->setText( "Reset to default values" );
      pb->setMaximumHeight( 22 );
      pb->show();
      connect( pb, SIGNAL( clicked() ), SLOT(module_reset_interact_interact_1() ) );
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

      QString id = "interact_1:_errorMessages";
      global_data_types[ id ] = "lbl";
      panel1_widget_map[ id ] = lbl;
      panel1_outputs.push_back( id );
      save_default_value( id );
   }
   ppos++;
   {
      QLabel * lbl = new QLabel( "Output value 1  ", this );
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

      QString id = "interact_1:output1";
      global_data_types[ id ] = "le";
      panel1_widget_map[ id ] = le;
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

void sassie::module_reset_interact_interact_1()
{
   reset_default_values();
}

void sassie::module_submit_interact_interact_1()
{
   save_last_values();
   reset_output_values( "default_value" );
   // qDebug( input_to_json( "interact_1" ) );
   // we should check if process already running

   QString program = "/src/sassie2/bin/interact_1";

   QFileInfo qfi( program );
   if ( !qfi.exists() || !qfi.isExecutable() || qfi.isDir() )
   {
      QString id = "interact_1";
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
   
   process_interact_interact_1 = new QProcess( this );
   process_interact_interact_1->addArgument( program );
   process_interact_interact_1->addArgument( input_to_json( "interact_1" ) );
   connect( process_interact_interact_1, 
            SIGNAL( readyReadStdout() ), 
            this, 
            SLOT( readyReadStdout_interact_interact_1() ) );
   connect( process_interact_interact_1, 
            SIGNAL( readyReadStderr() ), 
            this, 
            SLOT( readyReadStderr_interact_interact_1() ) );
   connect( process_interact_interact_1, 
            SIGNAL( launchFinished() ), 
            this, 
            SLOT( launchFinished_interact_interact_1() ) );
   connect( process_interact_interact_1, 
            SIGNAL( processExited() ), 
            this, 
            SLOT( processExited_interact_interact_1() ) );
   if ( !process_interact_interact_1->start() )
   { 
      qDebug( "error starting process" );
      return;
   }
   process_json[ "interact_1" ] = "";
   qDebug( "process started" );
}

void sassie::readyReadStdout_interact_interact_1()
{
   while ( process_interact_interact_1->canReadLineStdout() )
   {
      process_json[ "interact_1" ] += process_interact_interact_1->readLineStdout();
   }
}

void sassie::readyReadStderr_interact_interact_1()
{
   while ( process_interact_interact_1->canReadLineStderr() )
   {
      qDebug( process_interact_interact_1->readLineStderr() );
   }
}

void sassie::launchFinished_interact_interact_1()
{
   // qDebug( "process_interact_interact_1->launchFinished()" );
   disconnect( process_interact_interact_1,
               SIGNAL( launchFinished_interact_interact_1() ),
               0, 0 );
}

void sassie::processExited_interact_interact_1()
{
   qDebug( "process_interact_interact_1->processExited()" );
   readyReadStdout_interact_interact_1();
   readyReadStderr_interact_interact_1();

   disconnect( process_interact_interact_1, 0, 0, 0 );
   // post process data into output fields, add unexpected data etc
   // delete process_interact_interact_1;
   // process_interact_interact_1 = (QProcess *) 0;
   process_results( "interact_1" );
}
