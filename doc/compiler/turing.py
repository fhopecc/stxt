the01seq = {
('q1','B'):('P0','R','q2'),
('q2','B'):('PB','R','q3'),
('q3','B'):('P1','R','q4'),
('q4','B'):('PB','R','q1'),
}

copy = {
('s1','0'):('N','N','H'),
('s1','1'):('E','R','s2'),
('s2','0'):('E','R','s3'),
('s2','1'):('P1','R','s2'),
('s3','0'):('P1','L','s4'),
('s3','1'):('P1','R','s3'),
('s4','0'):('E','L','s5'),
('s4','1'):('P1','L','s4'),
('s5','0'):('P1','R','s1'),
('s5','1'):('P1','L','s5')
}

class TuringMachine:
    def __init__(self, start='s1', delta={}, tape='',
                 blank = 'B', halt = 'H'):
        self.halt = halt
        self.blank = blank

        self.state = start
        self.delta = delta
        self.position = 0
        self.number = 0
        
        self.tape = {}
        i = 0
        for c in tape:
            self.tape[i] = c
            i+=1

    @property
    def scan_symbol(self):
        try: 
            return self.tape[self.position]
        except:
            return self.blank

    def move(self):
        action = self.delta[(self.state, self.scan_symbol)]

        if action[0][0] == 'P':
            self.tape[self.position] = action[0][1]
        elif action[0][0] == 'E':
            self.tape[self.position] = self.blank

        if action[1] == 'R':
            self.position += 1
        elif action[1] == 'L':    
            self.position -= 1

        self.state = action[2]
        self.number += 1

    def next(self):
        try:
            self.move()
        except:
            pass

    def run(self):
        while 1 == 1:
            if self.state == self.halt:
                break
            self.move()
            self.print_tape()

    def print_tape(self):
        s = ''
        for k in self.tape:
            s += self.tape[k]
            if self.position == k + 1 :
                s += '[%s]' % self.state

        print '%d:%s' % (self.number, s)

t = TuringMachine(delta = copy, start = 's1', blank = '0', halt = 'H',
                  tape='1111')

t.run()
#for i in range(0,48):
#    t.print_tape()
#    t.next()
