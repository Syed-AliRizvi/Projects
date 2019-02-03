#reference shuffle: https://stackoverflow.com/questions/976882/shuffling-a-list-of-objects
#reference assert: https://wiki.python.org/moin/UsingAssertionsEffectively
#reference check object class: https://docs.python.org/2/tutorial/classes.html

from card import Card
from random import randint

class Deck:

    #object instantiation
    def __init__(self, value_start, value_end, number_of_suits):

        #defining list to fill with cards
        self.cards = []

        #through error if value start is not greater than zero or value end is smaller than value start
        assert value_start > 0 and value_end >= value_start, 'did not meet conditions: start_value>0 AND start_value >= end_value'

        #loop for each suit required
        for suit in range(1,number_of_suits+1):
            #for each suit loop for the number of cards required to be created
            for face in range(value_start, value_end+1):
                new_card = Card(face,suit)  #create Card instance
                self.cards.append(new_card) #append card to deck cards list

    #converts the object itself in form of string of cards per line
    def __str__(self):

        output_str = ''

        for card in self.cards:
            output_str += str(card) + '\n'

        output_str.strip('\n')
        return output_str

    #returns the list containing all cards
    def card_list(self):
        return self.cards

    #shuffle method for cards
    def shuffle_cards(self):

        #count nbr of cards in deck
        card_count = len(self.cards)
        #create clone of current cards
        copy_cards = self.cards[:]
        #empty current card list (clone is already created)
        self.cards = []

        #loop till the number of cards is not zero.
        while card_count != 0:
            #choose random integer to index cards; starts at 0 and max limit is the total cards in deck -1.
            card_index = randint(0, card_count-1)
            #add card of that random index to the deck cards.
            self.cards.append(copy_cards[card_index])
            #delete the transfered card from clone.
            del copy_cards[card_index]
            #since the card is deleted decrease the card count by 1.
            card_count -= 1


    #checks if a card is in the deck (true, false)
    def check_deck(self,the_face, the_suit):
        #loop over all cards in deck
        for card in self.cards:
            #if any card exists with the face and suit given return TRUE.
            if card.get_face() == the_face and card.get_suit() == the_suit:
                return True
        #returns false is no card exists with that face and suit
        return False


    #checks index of a card in deck
    def check_index(self,the_face, the_suit):
        #loop over each card in deck
        for card in self.cards:
            #if a card is found with matching face and suit return that cards index.
            if card.get_face() == the_face and card.get_suit() == the_suit:
                    return self.cards.index(card)
        #if no card exists with that face and suit return False.
        return False


    #adding new card to deck
    def add_card(self, the_face, the_suit):

        #if card is already in deck then do not add.
        if self.check_deck(the_face, the_suit) is True:
            return print('cannot ADD; card already in deck')
        #if card is not in deck then creat a new object instance and append it in deck cards list
        else:
            new_card = Card(the_face,the_suit)
            self.cards.append(new_card)

    #adding card instance of class Card
    def add_card_instance(self, the_card):
        #check if the class of the object getting inserted is indeed that of Card.
        if isinstance(the_card, Card):
            #if it is check if a card of that suit and face already exists in deck, if so do not add.
            if self.check_deck(the_card.get_face(), the_card.get_suit()) is True:
                return print('cannot ADD; card already in deck')
            #if card does not exist then append card to deck cards.
            else:
                self.cards.append(the_card)
        #if object is not of class Card then do not add.
        else:
            return print ('input is not an object of class Card')


    #draws a specific card
    def draw_card(self, the_face, the_suit):

        #check if card exists in deck by checking face and suit
        if self.check_deck(the_face, the_suit) is True:
            #if card exists, find its index
            draw_index = self.check_index(the_face, the_suit)
            #store card on that index; after that delete from deck cards.
            rm_card = self.cards[draw_index]
            del self.cards[draw_index]
            #give back the stored/deleted card.
            return rm_card
        #if no matching card; cannot draw.
        else:
            return print ('cannot Draw; card not in deck.')

    #draws card from top of the pile (end of list)
    def draw_top(self):
        #index of last card in deck/list
        draw_index = len(self.cards)-1
        #store card in that index then delete it from deck cards
        rm_card = self.cards[draw_index]
        del self.cards[draw_index]
        #give back stored/deleted card.
        return rm_card

    #draws card from bottom of the pile (begining of list)
    def draw_bottom(self):
        #index of top card in deck/list
        draw_index = 0
        #store card on that index then delete it from deck cards
        rm_card = self.cards[draw_index]
        del self.cards[draw_index]
        #give back stored/deleted card.
        return rm_card


def main():

    print('check init')
    my_deck = Deck(1,1,4)
    print(my_deck)

    print('check shuffle')
    my_deck.shuffle_cards()
    print (my_deck)

    print('check add new')
    my_deck.add_card('K', 'spade')
    print(my_deck)

    print('check add new object of class Card')
    my_new_card = Card(5,0)
    my_deck.add_card_instance(my_new_card)
    print(my_deck)

    print('check add existing object of class Card')
    my_new_card = Card(0,1)
    my_deck.add_card_instance(my_new_card)
    print(my_deck)

    print('check add new NOT object of class Card')
    my_new_card = Card(5,0)
    my_new = [5,0]
    my_deck.add_card_instance(my_new)
    print(my_deck)

    print('check add existing')
    my_deck.add_card('0', '1')
    print(my_deck)

    print('check drawing specific card')
    rem_card = my_deck.draw_card(the_face='0', the_suit='2')
    print (my_deck)
    print ( str(rem_card) + ' is now drawn' )

    print('check drawing from top')
    rem_card = my_deck.draw_top()
    print (my_deck)
    print (str(rem_card) + ' is now drawn')

    print('check drawing from bottom')
    rem_card = my_deck.draw_bottom()
    print (my_deck)
    print (str(rem_card) + ' is now drawn')

    print ('check drawing with incorrect arguments')
    rem_card = my_deck.draw_card(the_face='A', the_suit='heartttt')
    print (my_deck)
    print ( str(rem_card) + ' is now drawn' )


if __name__=='__main__':
    main()
