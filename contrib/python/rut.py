# -*- coding: utf-8 -*-
#
# RUT machine simulator - text mode
#
#   Author: Loïc Paulevé <loic AT inzenet DOT org>
#   Licence : BSD
#
# compatibility : unix (tested on linux and mac os x)
# requirements : python 2.5
#
# usage : python rut.py <rut file> <init pos> <init state> <letter0> <letter1> ... <letterN>
#    eg : python rut.py ruts/swap.rut 0 r a A A a a a a A
#
# keys : forward -> j; backward -> k; quit -> q; keep on screen: return
#
# revision: 1 (2008-02-17)
# - count support for commands j and k (eg: 10j)
# - anonymous states showed as "#<stateid>"
# - don't fail when incomplete or not reversible, just print
# revision: 0 (2008-02-16)
#

"""
check for reversibility and completeness is done when calling
 the rut.reverse method.
* reversibility is checked as follow :
	- reverse transitions
	- if conflict, rut is not reversible
* completeness is checked as follow :
	- after a reverse, there is a transition from all states
"""


import struct
import sys

#
# Core
#
class rut_configuration :
	def __init__( self, tape=None, head=None, state=None ) :
		self.tape = tape or []
		self.head = head or 0
		self.state = state or 0

class HaltException(Exception):
	pass

class rut :

	def step( self, cfg ) :
		i = self.instr[cfg.state]
		if i is None : # unknown instruction
			raise HaltException
		elif type(i) is dict : # matching instruction
			try :
				cfg.tape[cfg.head], sp = i[cfg.tape[cfg.head]]
			except KeyError :
				raise HaltException
			except IndexError : # out of tape
				raise HaltException
		else :
			d, sp = i
			cfg.head += d and 1 or -1
			if cfg.head < 0 :
				raise HaltException
		cfg.state = sp

	def reverse( self ) :
		""" check for reversibility, completeness and return the reversed rut """
		rev = self.__class__()
		# identical fields : letters and states symbols
		for field in ["N", "M", "tr_letter", "tr_state"] :
			setattr(rev, field, getattr(self, field))
		# reverse instr
		rev.instr = [None] * len(self.instr)
		s = 0
		is_reversible = True
		for i in self.instr :
			if type(i) is dict : # matching instruction
				for a, (b, sp) in i.iteritems() :
					ri = rev.instr[sp]
					if ri is None :
						rev.instr[sp] = {}
					else :
						if not (type(ri) is dict and b not in ri) :
							is_reversible = False
					rev.instr[sp][b] = (a, s)
			else :
				d, sp = i
				if rev.instr[sp] is not None :
					is_reversible = False
				rev.instr[sp] = (1-d, s)
			s += 1
		if None in rev.instr :
			print "Machine is not complete"
		if not is_reversible :
			print "Machine is not reversible"
		else :
			return rev

	def tofile( self, fd ) :
		# matching table
		fd.seek(16 + self.N*4)
		for j in xrange(self.N) :
			if type(self.instr[j]) is dict :
				offset = fd.tell()
				assert offset % 2 == 0
				for a, (b, sp) in self.instr[j].iteritems() :
					fd.write(struct.pack(">2l", (a<<16) + (b<<1) + 1, sp))
				fd.write(struct.pack(">l", 0)) # EOM
				self.instr[j] = offset

		# letters symbol table
		offset_letters = fd.tell()
		fd.write("".join(["%s\0" % l for l in self.tr_letter]))
		# states symbol table
		states = ""
		s = 0
		for tr in self.tr_state :
			if tr != "#%d" % s :
				fd.write(struct.pack(">l", s)+tr+"\0")
			s += 1

		# header
		fd.seek(0)
		fd.write(struct.pack(">4l", 0x7275740a, self.M, self.N, offset_letters))

		# instructions
		for i in self.instr :
			if type(i) is tuple : # movement
				d, sp = i
				fd.write(struct.pack(">l", 4*sp+2*d+1))
			else : # matching offset
				fd.write(struct.pack(">l", i))


	@classmethod
	def fromfile( celf, fd ) :
		self = celf()

		def nunpack( n ) :
			return struct.unpack(">%dl"%n, fd.read(n*4)) # signed long

		# header
		magic, self.M, self.N, offset_symb = nunpack(4)
		assert magic == 0x7275740a, "not a rut file format"

		# matching table
		def read_matching_list( offset ) :
			fd.seek(offset)
			mt = []
			nab = nunpack(1)[0]
			while nab != 0 :
				ab, (s, nab) = nab, nunpack(2)
				mt.append((ab>>16, ((ab>>1) & 0x7fff, s)))
			return dict(mt)

		# instructions table
		def parse_instr( w ) :
			if w % 2 == 1 :
				return (w>>1) & 1, w>>2
			else :
				return read_matching_list(w)
		self.instr = [parse_instr(w) for w in nunpack(self.N)]

		def read_string() :
			d, c = "", ""
			while c != "\0" :
				d += c
				c = fd.read(1)
				assert c != "", "error reading file"
			return d
		fd.seek(offset_symb)
		# letters symbol table
		self.tr_letter = [read_string() for i in xrange(self.M)]
		# states symbol table
		self.tr_state = ["#%d"%s for s in xrange(self.N)]
		try :
			while True :
				s = nunpack(1)[0]
				self.tr_state[s] = read_string()
		except struct.error :
			pass
		return self

	def __str__( self ) :
		return "\n".join([str(self.instr), str(self.tr_letter), str(self.tr_state)])

#
# End Core
#

#
# UI utilities
#
import array
import fcntl
import signal
import termios
import tty

## start getch code (from http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/134892)
# unix code
def getch() :
	fd = sys.stdin.fileno()
	old_settings = termios.tcgetattr(fd)
	try:
		tty.setraw(sys.stdin.fileno())
		ch = sys.stdin.read(1)
	finally:
		termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
	return ch
## end getch code

class display :
	""" the line displayed on screen """
	def __init__( self ) :
		self.fd = sys.stdout
		self.detect_size()
		signal.signal(signal.SIGWINCH, self.detect_size)

	def detect_size( self ) :
		h, self.width = array.array('h', fcntl.ioctl(self.fd, termios.TIOCGWINSZ, '\0'*8))[:2]

	def set( self, data ) :
		""" erase current line and print data instead """
		data = data.ljust(self.width)
		self.fd.write(data+"\r")

#
# End UI utilities
#


if __name__ == "__main__" :
	args = sys.argv[1:]

	print "usage: <rut file> <init pos> <init state> <letter0> <letter1> ... <letterN>"

	rutfile = args.pop(0)
	m = rut.fromfile(open(rutfile))
	#print m
	mp = m.reverse()

	pos, state, tape = int(args[0]), m.tr_state.index(args[1]), [m.tr_letter.index(l) for l in args[2:]]

	cfg = rut_configuration(tape=tape, state=state, head=pos)

	tick = 0
	def forward() :
		global tick
		tick += 1
		m.step(cfg)
	def backward() :
		global tick
		tick -= 1
		mp.step(cfg)

	d = display()

	state_max_len = max(map(len, m.tr_state))
	show_fmt = "%%10d (%%%ds) #%%c%%s" % state_max_len

	def show() :
		first_blank = " "
		str_tape = [m.tr_letter[l]+" " for l in cfg.tape]
		if cfg.head < len(str_tape) :
			if cfg.head > 0 :
				str_tape[cfg.head-1] = str_tape[cfg.head-1][:-1]+"("
			else :
				first_blank = "("
			str_tape[cfg.head] = str_tape[cfg.head][:-1]+")"
		else :
			str_tape[-1] = str_tape[-1][:-1]+"("
			str_tape.append(")")
		d.set(show_fmt % (tick, m.tr_state[cfg.state], first_blank, "".join(str_tape)))

	if mp :
		print "k: backward;",
	print "j: forward; q: quit; return: keep on screen"
	count = ""
	try :
		k = None
		show()
		while k != "q" :
			k = getch()
			if mp and k == "k" :
				for i in xrange(int(count or 1) or 1) :
					backward()
					show()
				count = ""
			elif k == "j" :
				for i in xrange(int(count or 1) or 1) :
					forward()
					show()
				count = ""
			elif k in ["\r"] :
				print
				show()
				count = ""
			#elif k in ["\x1b"] : # escape
			#	count = ""
			elif k in ["0","1","2","3","4","5","6","7","8","9"] :
				count += k
			else :
				count = ""
	except HaltException :
		pass
	print

