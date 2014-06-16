import ConfigParser

DEF_FILE = 'ems_db.cfg'

def config(default = False, host = None, port = None, user = None, pwd = None, 
        db = None, config_file = None, main_path = None):

    """Stores database connection parameters in a configuration file
    """

    config = ConfigParser.RawConfigParser()

    if default is True:

        if host is None:
            host = "127.0.0.1"
        if port is None:
            port = 3306
        if user is None:
            user = "root"
        if pwd is None:
            pwd = ""
        if db is None:
            db = "ems"
        if main_path is None:
            main_path = "/Volumes/IOMEGA_LIL/Electronic Music Artists/"

    config.add_section('MySQL')

    if host is not None:
        config.set('MySQL', 'HOST', host)
    if port is not None:
        config.set('MySQL', 'PORT', port)
    if user is not None:
        config.set('MySQL', 'USER', user)
    if pwd is not None:
        config.set('MySQL', 'PWD', pwd)
    if db is not None:
        config.set('MySQL', 'DB', db)

    config.add_section('Files')
    
    if main_path is not None:
        config.set('Files', 'MAIN_PATH', main_path)
    
    if config_file is None:
        config_file = DEF_FILE

    with open(config_file, 'wb') as f:
        config.write(f)

def read_config(config_file = None, section = 'MySQL'):

    """Reads database connection parameters from a configuration file
    """



    if config_file is None:
        config_file = DEF_FILE
	
    config = ConfigParser.RawConfigParser()
    config.read(config_file)

    if section is 'MySQL':

        host = config.get('MySQL', 'HOST')
        port = config.getint('MySQL', 'PORT')
        user = config.get('MySQL', 'USER')
        pwd = config.get('MySQL', 'PWD')
        db = config.get('MySQL', 'DB')

        c = {'host': host, 'port': port, 'user': user, 'pwd': pwd, 'db':db}

    elif section is "Files":

        main_path = config.get('Files', 'MAIN_PATH')

        c = {'main_path': main_path}

    return c


