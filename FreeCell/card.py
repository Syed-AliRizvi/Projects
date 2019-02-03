#reference setattr: https://stackoverflow.com/questions/8187082/how-can-you-set-class-attributes-from-variable-arguments-kwargs-in-python
#reference .items(): https://stackoverflow.com/questions/10458437/what-is-the-difference-between-dict-items-and-dict-iteritems

class Card:

    #constructor
    def __init__(self,the_face, the_suit, the_color = None):

        self.face = str(the_face)
        self.suit = str(the_suit)
        self.color = the_color if the_color == None else str(the_color) #stays as default or if speficied then store it as str

    #magic function to overload string for object
    def __str__(self):
        #cannot cut down and take only one digit of face because face is an integer 1 and 11 will be shown as 1:1 and 1:1 as well.
        return str(self.face) + ':' + str(self.suit[0]) #reference Gavin's lecture style | convert to str as in some games it might not be str

    #accessors
    def get_face(self):
        return self.face

    def get_suit(self):
        return self.suit

    def get_color(self):
        return self.color

    #mutators; converted to string as the entry might not be string
    def change_face(self, new_face):
        self.face = str(new_face)

    def change_suit(self, new_suit):
        self.suit = str(new_suit)

    def change_color(self, new_color):
        self.color = str(new_color)


def main():
    fcard = Card('Ace', 'spade')

    print('checking object print')
    print(fcard)

    print('checking object face')
    print(fcard.get_face())

    print('checking object suit')
    print(fcard.get_suit())

    print('checking object color')
    print(fcard.get_color())

    print('changing object face')
    fcard.change_face('hello')
    print(fcard)

    print('changing object suit')
    fcard.change_suit('diamond')
    print(fcard)

    print('chaging object color')
    fcard.change_color('red')
    print(fcard.get_color())




if __name__ == '__main__':
    main()
