#include <termios.h>
#include <unistd.h>

#import "initserial.h"

// A simple function which initializes serial port device: 8 bit data, one stop bit, RTS/CTS flow control

int initSerialPort(int fd) {
	struct termios params;
	
	tcflush(fd, TCIOFLUSH);
	
	if (tcgetattr(fd, &params) == -1) return -1;
	
	cfmakeraw(&params);
#ifdef __MAX_BAUD
	cfsetspeed(&params, __MAX_BAUD);
#elif defined B230400
	cfsetspeed(&params, B230400);
#endif
	params.c_cflag = CLOCAL | CREAD | CS8 | HUPCL | CRTSCTS;
	
	//params.c_cc[VTIME] = (5000 + 50) / 100;
    //params.c_cc[VMIN] = 0;
	
	if (tcsetattr(fd, TCSANOW, &params) == -1) return -1;
	
	return 0;
}
