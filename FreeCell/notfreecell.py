#ref using zip for print: https://stackoverflow.com/questions/35580061/print-lists-in-a-list-in-columns
#ref using continue for restarting loop: https://stackoverflow.com/questions/3704918/python-way-to-restart-a-for-loop-similar-to-continue-for-while-loops

from deck import Deck

class NotFreeCell:

    #constructor
    def __init__(self):

        #creating lists for foundataions; these would be filled later

        #foundation lists are created as a list of string and a list.
        #the list contains the cards in foundations while the string identifies if the suit for which the foundation is for.
        #the suit is used for rule checks when placing cards.
        #Because of the list in list removing a card from foundation is different from other lists.
        self.s_foundation = ['s',[]]
        self.h_foundation = ['h',[]]
        self.d_foundation = ['d',[]]
        self.c_foundation = ['c',[]]

        #creating lists for cell slots; these would be filled later
        self.cell_slot_1 = []
        self.cell_slot_2 = []
        self.cell_slot_3 = []
        self.cell_slot_4 = []

        #creating lists for cascades; these would be filled later
        self.col_1 = []
        self.col_2 = []
        self.col_3 = []
        self.col_4 = []
        self.col_5 = []
        self.col_6 = []
        self.col_7 = []
        self.col_8 = []

        #creating a list of lists of each type of games list
        #these would be used to easily check if a list is a cascade, foundation or cell slot
        #these would also be used to check which list is required by requiring input from player and using indexing on these.
        self.all_col = [self.col_1, self.col_2, self.col_3, self.col_4, self.col_5, self.col_6, self.col_7, self.col_8]
        self.all_cs = [self.cell_slot_1, self.cell_slot_2, self.cell_slot_3, self.cell_slot_4]
        self.all_fd = [self.c_foundation, self.d_foundation, self.s_foundation, self.h_foundation]

        #dictioary that contains the integer value for each card.
        #this would make comparing cards easier
        self.card_int = dict()

        #create deck to play FreeCell with.
        #customize deck to give it the required suits, colors, faces, and assign int values
        my_deck = Deck(1, 13, 4)

        #iterate over each card in deck; use mutators to change color, face, suit for each card and assign it an integer value for hierarchy.
        for card in my_deck.card_list():

            #defining suits and color
            if card.get_suit() == '1':
                card.change_suit('spade')   #1st suit is assigned suit spade by mutating card suit
                card.change_color('black')  #1st suit is assigned color black by mutating card color
            elif card.get_suit() == '2':
                card.change_suit('club')    #2nd suit is assigned suit club by mutating card suit
                card.change_color('black')  #2nd suit is assigned color back by mutating card color
            elif card.get_suit() == '3':
                card.change_suit('heart')   #3rd suit is assigned suit heart by mutating card suit
                card.change_color('red')    #3rd suit is assigned color red by mutating card color
            else:
                card.change_suit('diamond') #4th suit is assigned suit diamond by mutating card suit
                card.change_color('red')    #4th suit is assigned color red by mutating card color

            #defining face and putting int value of face in dict
            if card.get_face() == '1':
                card.change_face('A')
                self.card_int[str(card)]=1
            elif card.get_face() == '10':
                card.change_face('X')
                self.card_int[str(card)]=10
            elif card.get_face() == '11':
                card.change_face('J')
                self.card_int[str(card)]=11
            elif card.get_face() == '12':
                card.change_face('Q')
                self.card_int[str(card)]=12
            elif card.get_face() == '13':
                card.change_face('K')
                self.card_int[str(card)]=13
            else:
                self.card_int[str(card)]=int(card.get_face())


        #shuffle cards before dealing into game.
        my_deck.shuffle_cards()

        #populating the cascade using deck.
        card_in_col = 7 #number of cards to be dealt in each column
        col_counter = 1 #counter of columns dealt in.

        #loop over each col in cascade (total 8 cols)
        for col in self.all_col:

            #if the number of cols reaches 5 change the number of cards in col to be 6 from 7.
            if col_counter  == 5:
                card_in_col = 6

            #loop created to loop the same amount of times as the nbr of cards that need to be dealt in column.
            #in each loop draw card from deck and append it in column.
            cards_dealt = 0
            while cards_dealt < card_in_col:
                card = my_deck.draw_top()
                col.append(card)
                cards_dealt += 1 #increase cards dealt count

            col_counter += 1 #increase col count

    #defining str to print game in correct form.
    def __str__(self):

        #print would be done by zipping all columns.
        #a loop is used to append spaces to cascade cols to make them equal to the max length.
        max_len = max( len(col) for col in self.all_col )
        for col in self.all_col:
            if len(col) < max_len:
                for i in range(max_len-len(col)):
                    col.append(' ')

        #zip coloumns to create list of rows.
        zip_tuples = zip (self.col_1, self.col_2, self.col_3, self.col_4, self.col_5, self.col_6, self.col_7, self.col_8)
        list_zip_tuples = list(zip_tuples)
        cascade_str = ''

        #pretty print format for rows; reference Gavin.
        for row in list_zip_tuples:
            for card in row:
                cascade_str += (str(card) + '\t\t')
            cascade_str += '\n'

        #remove spaces added for zip print
        for col in self.all_col:
            while ' ' in col:
                col.remove(' ')

        # since we dont know which ones empty and which ones not and we need to create a string with the correct form we first initialize both cs and foundations
        #this is done so that we do not get an index error when printing last value in list if the list is empty
        self.s_fl = str(self.s_foundation[1][-1]) if self.s_foundation[1] else ' S'
        self.h_fl = str(self.h_foundation[1][-1]) if self.h_foundation[1] else ' H'
        self.d_fl = str(self.d_foundation[1][-1]) if self.d_foundation[1] else ' D'
        self.c_fl = str(self.c_foundation[1][-1]) if self.c_foundation[1] else ' C'

        self.cs_1 = str(self.cell_slot_1[0]) if self.cell_slot_1 else ''
        self.cs_2 = str(self.cell_slot_2[0]) if self.cell_slot_2 else ''
        self.cs_3 = str(self.cell_slot_3[0]) if self.cell_slot_3 else ''
        self.cs_4 = str(self.cell_slot_4[0]) if self.cell_slot_4 else ''

        #defining string variables to be used to create the print format.
        cell_slots  = str(self.cs_1) + '\t\t' + str(self.cs_2) + '\t\t' + str(self.cs_3) + '\t\t' + str(self.cs_4)

        foundations = str(self.c_fl) + '\t\t' + str(self.d_fl) + '\t\t' + str(self.s_fl) + '\t\t' + str(self.h_fl)

        cells_found_heading = 'c1\t\tc2\t\tc3\t\tc4' +'\t\t' + 'f1\t\tf2\t\tf3\t\tf4'

        line_break = '---\t\t---\t\t---\t\t---\t\t---\t\t---\t\t---\t\t---'

        cascade_heading = 'c1\t\tc2\t\tc3\t\tc4\t\tc5\t\tc6\t\tc7\t\tc8'

        #return a string in the correct formating for the game.
        return  (
                cells_found_heading + '\n' + \
                line_break + '\n' + \
                cell_slots + '\t\t' + foundations + '\n' + \
                line_break + '\n\n\n' + \
                cascade_heading + '\n' + \
                line_break + '\n' + \
                cascade_str
                )



    #get the int value of the card using the dictionary defined earlier
    def card_val(self, card):
        return self.card_int[str(card)]


    #moving a card from one list to another using FreeCell rules
    def move_card(self, from_list, to_list):

        #from_list extraction
        if from_list:
            if from_list in self.all_fd: #if from list is a foundation list
                mv_card = from_list[1][-1]
            else:
                mv_card = from_list[-1] #if from list is not a foundation list
        else:
            return print('no card to remove, empty slot')

        #to_list checking and putting

        #putting in foundations
        if to_list in self.all_fd:

            #first check if foundation of correct suit
            #this is done by comparing the string suit in the foundations list at index 0.
            if str(mv_card)[-1] == to_list[0]:

                #if foundation is empty
                if not to_list[1]:
                    #check if the card is A
                    if mv_card.get_face() == 'A':
                        #since foundataions have two lists in list hence a simple pop would return a list of cards and not a card, hence if statement is used to check.
                        from_list.pop() if from_list not in self.all_fd else from_list[1].pop()
                        return to_list[1].append(mv_card)
                    #if foundation is empty but card is not A then it cannot be placed.
                    else:
                        return print ('cannot place card on foundation, first card needs to be "A"')

                #if foundation has cards
                else:
                    #check if the move card is exactly one ABOVE the value of the other card (placing on foundation)
                    #the card_value dictionary is used for this purpose.
                    if self.card_val(mv_card) - self.card_val(to_list[1][-1]) == 1:
                        from_list.pop()  if from_list not in self.all_fd else from_list[1].pop()
                        return to_list[1].append(mv_card)
                    else:
                        #if card is not in correct sequence, it cannot be placed.
                        return print('cannot place card on foundation, not in sequence')

            else:
                return print('cannot place card on foundation, suit does not match')

        #putting in cell slot; first check if the to list is in the cell slot lists
        elif to_list in self.all_cs:
            #if the target list is empty; append the card and remove the card from old list accordingly.
            if not to_list:
                from_list.pop()  if from_list not in self.all_fd else from_list[1].pop()
                return to_list.append(mv_card)
            #if target list has a card, cannot place another card.
            else:
                return print('cell slot taken')

        #putting in cascade
        else:
            #if cascade is empty, simply append without check; pop is done accordingly like always.
            if not to_list:
                from_list.pop() if from_list not in self.all_fd else from_list[1].pop()
                return to_list.append(mv_card)

            #if cascade has cards
            else:

                #if cascade has cards, color or target and move card should be different
                if mv_card.get_color() != to_list[-1].get_color():
                    #the value of the move card must be exactly one LESS than target card; if so append it and remove accordingly.
                    if self.card_val(mv_card) - self.card_val(to_list[-1]) == -1:
                        from_list.pop() if from_list not in self.all_fd else from_list[1].pop()
                        return to_list.append(mv_card)
                    #if incorrect sequence; cannot place card
                    else:
                        return print ('card cannot be placed on cascade, card not in sequence')
                #if same color cards; then card cannot be placed.
                else:
                    return print ('card cannot be placed on cascade, incorrect color')

    #checks if teh game has been won
    def game_won(self):

        i=0 #checker for foundations with 13 cards.
        #for all foundations
        for foundation in self.all_fd:
            #if length of foundation is 13 (all cards are there for the suit) increase checker by 4
            if len(foundation) == 13:
                i += 1
        #if the checker value is 4; meaning all foundations have all 13 cards for their suits break the loop
        return True if i == 4 else False

    #a function created that contains a menu in order for the player to play an instance of the class NotFreeCell
    def play(self):

        #predefining option strings
        area_options = '1 for cascades, 2 for cell slots, 3 for foundations, q to quit'
        cascade_options = '1-8 for cascades 1-8'
        cs_options = '1-4 for cell slots 1-4'
        f_options = '1-4 for foundations 1-4'

        #print welcome message and end with multiple line space
        print ('\n' + 'Welcome to NotFreeCell!\n\n\n')

        #print the current state of the game
        print (self)

        #the options and game continue till the player quits; breaks loop
        while True:

            #print the main options; which area does the player want to move from or if quit: Cascade, Cell Slot, or Foundation, or Quit
            print ('key: ' + area_options +'\n')

            #IDENTIFY LIST WHERE TO MOVE CARD FROM

            #store the input
            area_chosen = input('specify where to move from: ')

            #if input q then break loop
            if area_chosen == 'q':
                print ('spoil sport')
                break

            #if input is for cascades
            elif area_chosen == '1':
                #print options for cascades then require which cascade input
                print ('key: ' + cascade_options)
                from_option = input ('specify option: ')
                #if incorrect option print error message and restart the loop (input options)
                if from_option not in ['1', '2', '3','4','5','6','7','8']:
                    print ('\n\n'+ str(self) + '\n\n***input error, start again***\n\n')
                    continue
                #if options are correct then store from list by indexing the all cols (cascades) list
                from_list = self.all_col[int(from_option)-1]

            #if input is for cell slots
            elif area_chosen == '2':
                #print options then require which cell slot input
                print('key: ' + cs_options)
                from_option = input ('specify option: ')
                #if incorrect option then print error message and restart loop (input options)
                if from_option not in ['1', '2', '3', '4']:
                    print ('\n\n'+ str(self) + '\n\n***input error, start again***\n\n')
                    continue
                #if options are correct then store from list by indexing from all cell slot list
                from_list = self.all_cs[int(from_option)-1]

            #if input is for foundations
            elif area_chosen == '3':
                #print options then require which foundation input
                print('key: ' + f_options)
                from_option = input ('specify option: ')
                #if incorrect option then print error message and restart loop (input options)
                if from_option not in ['1', '2', '3', '4']:
                    print ('\n\n'+ str(self) + '\n\n***input error, start again***\n\n')
                    continue
                #if options are correct then store from list by indexing from all foundations list
                from_list = self.all_fd[int(from_option)-1]

            #if area chosen is incorrect then print error message and restart loop (input options)
            else:
                print ('\n\n'+ str(self) + '\n\n***input error, start again***\n\n')
                continue


            #IDENTIFY LIST WHERE TO MOVE CARD TO

            #store area chosen to move from: cascade, cell slot, foundataion or quit.
            area_chosen = input('\n'+'specify where to move to: ')

            #if quit, break loop
            if area_chosen == 'q':
                print ('spoil sport.')
                break

            #if option is for cascades
            elif area_chosen == '1':
                #print options and require input of column to move to
                print ('key: ' + cascade_options)
                to_option = input ('specify option: ')
                #if incorrect option print error message and restart loop (input options)
                if to_option not in ['1', '2', '3','4','5','6','7','8']:
                    print ('\n\n'+ str(self) + '\n\n***input error, start again***\n\n')
                    continue
                #if correct option then store to list by indexing all cols list.
                to_list = self.all_col[int(to_option)-1]

            #if option is for cell slots
            elif area_chosen == '2':
                #print options and require input of column to move to
                print('key: ' + cs_options)
                to_option = input ('specify option: ')
                #if incorrect option print error message and restart loop (input options)
                if to_option not in ['1', '2', '3', '4']:
                    print ('\n\n'+ str(self) + '\n\n***input error, start again***\n\n')
                    continue
                #if correct option then store to list by indexing all cell slot list.
                to_list = self.all_cs[int(to_option)-1]

            #foundations
            elif area_chosen == '3':
                #print options and require input of column to move to
                print('key: ' + f_options)
                to_option = input ('specify option: ')
                #if incorrect option print error message and restart loop (input options)
                if to_option not in ['1', '2', '3', '4']:
                    print ('\n\n'+ str(self) + '\n\n***input error, start again***\n\n')
                    continue
                #if correct option then store to list by indexing all cell slot list.
                to_list = self.all_fd[int(to_option)-1]

            #if area chosen is incorrect then print error message and restart loop.
            else:
                print ('\n\n'+ str(self) + '\n\n***input error, start again***\n\n')
                continue

            #call move card function by using the from_list and to_list taken from player
            print('\n')
            #the error prints will get printed if the there are rule breaks for FreeCell when moving cards from/to lists
            self.move_card(from_list,to_list)
            print('\n\n')

            #print the current status of the game
            print( self )

            if self.game_won():
                print('\n\nWINNER!!!!!')
                break



def main():

    game_to_play = NotFreeCell()
    game_to_play.play()

if __name__ == '__main__':
    main()
