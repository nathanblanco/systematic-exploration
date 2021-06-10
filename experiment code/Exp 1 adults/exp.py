#! C:\\python27\python



# differences between adults and children versions:
# -meter has 1000 points for adults rather than the 900 for children
# since adults will likely go over 900
# -goal is every 200 instead of 180 because of meter change
# -instructions are appropriately different
# -adults use mouse instead of touchscreen
# - adults do not win stickers :(

#------------------------------------------------------------
# import modules
#------------------------------------------------------------
import os, sys, signal
import math
from string import *
from numpy import *
from ftplib import FTP
from random import random, randint, shuffle, normalvariate
import pygame
import datetime
from pygame.locals import *
import tempfile
from time import sleep
from lib.pypsyexp import *
import eztext


execfile('text.py')
#execfile('inst_text.py')



#------------------------------------------------------------
# Define some global settings
#------------------------------------------------------------
LAPTOPRES = (1024,768)
FULLSCREENRES = (1024, 768)

NEXT = 1
BACK = 0

#
experimentname = 'Exploration-Development'
laptop = False
paid = True
ntrials = 100 # number of trials of the main task
goal_every = 200 # number of points needed to reach each goal

            
#if laptop:
screenres = LAPTOPRES
    #else:
    #    screenres = FULLSCREENRES

# colors we might use
white = (255, 255, 255)
grey = (175,175,175)
boxgrey = (128,128,128)
black = (0, 0, 0)

blue = (30, 170, 250)
green = (0,175,0)
red = (175, 0, 0)
yellow = (255, 215, 0)

oxygreen = (22,71,8)
screengreen = (173,198,156)
ltgrey = (193,193,193)
divred = (102,63,62)

transparent = (128, 128, 128)

# set whether debugging on laptop, or running in fullscreen
laptop = False

#------------------------------------------------------------
# MouseButton Classes:
# general code that makes the clickable buttons
#------------------------------------------------------------

# buttons for moving to next page of instructions
class NextButton(MouseButton):
    def __init__(self, x, y, w, h, myimage, snd, app):
        # This is how you call the superclass init
        MouseButton.__init__(self, x, y, w, h)
        self.image = myimage
        self.snd = snd
    def draw(self, surface):
        self.image_rect = self.image.get_rect()
        self.image_rect.center = self.rect.center
        surface.blit(self.image,self.image_rect)
    def do(self):
        self.snd.play()

# buttons for getting responses during the task
class RespButton(MouseButton):
    def __init__(self, x, y, w, h, id, myimage, myimagepressed, snd, presstime, app):
        # This is how you call the superclass init
        MouseButton.__init__(self, x, y, w, h)
        self.myid = id
        self.image = myimage
        # changed this to a random color so it wouldn't make my white backgrounds transparent
        self.image.set_colorkey((0,12,45))
        self.imagep = myimagepressed
        self.imagep.set_colorkey((0,12,45))
        self.snd = snd
        self.presstime = presstime
    def draw(self, surface):
        self.image_rect = self.image.get_rect()
        self.image_rect.center = self.rect.center
        surface.blit(self.image,self.image_rect)
    def do(self, surface):
        self.image_rect = self.imagep.get_rect()
        self.image_rect.center = self.rect.center
        surface.blit(self.imagep,self.image_rect)
        pygame.display.flip()
        self.snd.play()
        pygame.time.wait(self.presstime) #
        self.image_rect = self.image.get_rect()
        self.image_rect.center = self.rect.center
        surface.blit(self.image,self.image_rect)
        pygame.display.flip()
        return self.myid

#------------------------------------------------------------
# ExplDev Class
#------------------------------------------------------------
class ExplDevExp(Experiment):
    def __init__(self, laptop, screenres, experimentname):
        
        # inititialize everything
        self.experimentname = experimentname
        
        Experiment.__init__(self, laptop, screenres, experimentname)
        self.load_all_resources('images', 'sounds')
        
        #[self.cond, self.ncond, self.subj] = self.get_cond_and_subj_number('patterncode.txt')
        self.cond = 0 # no conditions in this version
        self.subj = self.record_subj()
        
        self.filename = "data/%s.dat" % self.subj
        self.datafile = open(self.filename, 'w')
        
        # log start time in the datafile
        self.output_trial([datetime.datetime.now().ctime()])
        
        # initialize total points
        self.earnings = 0
        
        # initialize goal tracker
        self.goal_reached = 0
        
        self.trial = 1
        
        # Locations of the creatures 
        # The first two are xy coords with origin at top-left, 
        # the next two are for coordinates with origin in the center of the screen
        self.locations = [ [80, 105, -375, -210], 
                           [80, 410, -375, 95], 
                           [450, 105, 0, -210], 
                           [450, 410, 0, 95] ]

        # Locations of the response options for the final test phase
        self.test_locations = [ [80, 510, -375, 195], 
                                [250, 510, -145, 195], 
                                [420, 510, 45, 195], 
                                [590, 510, 215, 195],
                                [760, 510, 385, 195]]


        # list of the stimuli images
        self.creatures = ['creature1.png', 'creature2.png', 'creature3.png', 'creature4.png']
        
        # list of the reward values for the four options
        self.rewards = [10, 3, 2, 1]
        
        # randomly assign creatures to locations
        shuffle(self.creatures)
        
        # randomly assign reward values to locations/creatures
        shuffle(self.rewards)
        
    
    #-------------------------------------------------------------
    # record_subj:
    # This allows the subject number to be input at the start of the experiment
    #-------------------------------------------------------------
    def record_subj(self):
        background = self.show_centered_image('instructions-background.gif',black)

        last_txtbox_value = None
        txtbx = eztext.Input(maxlength=45, color=(0,255,0), prompt='Subject #: ')
        # main loop!

        while 1:
            # events for txtbx
            events = pygame.event.get()
            # update txtbx
            txtbx.update(events, background)
            # blit txtbx on the sceen

            if((len(txtbx.value) > 0) and (txtbx.value[-1] == 'R')): break

            if(txtbx.value != last_txtbox_value):
                
                self.update_display(background)
                txtbx.draw(background)
                self.update_display(background)
                last_txtbox_value = txtbx.value

        return last_txtbox_value
    
    #------------------------------------------------------------
    # show_instructions:
    # displays instruction screens
    #------------------------------------------------------------
    def show_instructions(self, filename, butfn, butfn2):
        background = self.show_centered_image(filename, black)
        self.screen.blit(background, (0,0))
        self.button = NextButton(640, 725, 265, 50, self.resources[butfn],self.resources["buttonpress.wav"], self)
        self.button.draw(self.screen)
        if butfn2 != None:
            self.button2 = NextButton(140, 725, 265, 50, self.resources[butfn2],self.resources["buttonpress.wav"], self)
            self.button2.draw(self.screen)
        pygame.display.flip()
        
        time_stamp = pygame.time.get_ticks()
        
        retval = NEXT
        exit = False;
        while not exit:
            for event in pygame.event.get():
                if event.type == QUIT:
                    self.on_exit()
                elif event.type == KEYDOWN:
                    if event.key == K_ESCAPE:
                        self.on_exit()
                elif event.type == MOUSEBUTTONDOWN:
                    #print("here")
                    (x,y) = pygame.mouse.get_pos()
                    if (self.button.containsPoint(x, y)):
                        self.button.do()
                        exit = True
                        retval = NEXT
                    if butfn2 != None:
                        if (self.button2.containsPoint(x, y)):
                            self.button2.do()
                            exit = True
                            retval = BACK
        rt = pygame.time.get_ticks() - time_stamp
        return retval
    
    #------------------------------------------------------------
    # final_screen:
    # displays the final screen at the end of the experiment
    #------------------------------------------------------------
    def final_screen(self):
        
        background = self.clear_screen(white)
        
        end_text = "Wow! You collected %s candies! That's a lot! Please let the experimenter know you're done." % self.earnings
        self.place_text_image(background, end_text, 32, 0, -100, black, white)
        
        end_text2= "Thanks for playing! (Experimenter: press 'shift + ~' to close the experiment)"
        self.place_text_image(background, end_text2, 32, 0, 50, black, white)
        
        # output time exp finished to the end of the datafile
        self.output_trial([(datetime.datetime.now().ctime())])
        
        self.update_display(background)
        
        while 1:
            res = self.get_response()

    
    #------------------------------------------------------------
    # draw_buttons:
    # creates the clickable creature buttons
    #------------------------------------------------------------
    def draw_buttons(self, mysurf):
    
        # randomize what color candy is displayed during reward presentation
        candy_type = ['A','B','C','D'][randint(0,3)]
    
        self.buttons = []
        self.buttons = self.buttons + [RespButton(self.locations[0][0], self.locations[0][1], 360, 250, 0, self.resources[self.creatures[0]], self.resources[str(self.rewards[0])+'candy'+candy_type+'.png'],self.resources["recvdata.wav"], 3000, self)]
        self.buttons = self.buttons + [RespButton(self.locations[1][0], self.locations[1][1], 360, 250, 1, self.resources[self.creatures[1]], self.resources[str(self.rewards[1])+'candy'+candy_type+'.png'],self.resources["recvdata.wav"], 3000, self)]
        self.buttons = self.buttons + [RespButton(self.locations[2][0], self.locations[2][1], 360, 250, 2, self.resources[self.creatures[2]], self.resources[str(self.rewards[2])+'candy'+candy_type+'.png'],self.resources["recvdata.wav"], 3000, self)]
        self.buttons = self.buttons + [RespButton(self.locations[3][0], self.locations[3][1], 360, 250, 3, self.resources[self.creatures[3]], self.resources[str(self.rewards[3])+'candy'+candy_type+'.png'],self.resources["recvdata.wav"], 3000, self)]
        
        for i in self.buttons:
            i.draw(mysurf)
            
            
    #------------------------------------------------------------
    # draw_test_buttons:
    # creates the clickable choice options for the final test phase
    #------------------------------------------------------------
    def draw_test_buttons(self, mysurf, candy_type):
    
        # randomize locations of the five choice options
        candy_num_list = ['1', '2', '3', '10', '15']
        shuffle(candy_num_list)
        
        self.buttons = []
        self.buttons = self.buttons + [RespButton(self.test_locations[0][0], self.test_locations[0][1], 150, 115, candy_num_list[0], self.resources[candy_num_list[0]+'candy'+candy_type+'-0.png'], self.resources[candy_num_list[0]+'candy'+candy_type+'-0.png'],self.resources["buttonpress.wav"], 100, self)]
        self.buttons = self.buttons + [RespButton(self.test_locations[1][0], self.test_locations[1][1], 150, 115, candy_num_list[1], self.resources[candy_num_list[1]+'candy'+candy_type+'-0.png'], self.resources[candy_num_list[1]+'candy'+candy_type+'-0.png'],self.resources["buttonpress.wav"], 100, self)]
        self.buttons = self.buttons + [RespButton(self.test_locations[2][0], self.test_locations[2][1], 150, 115, candy_num_list[2], self.resources[candy_num_list[2]+'candy'+candy_type+'-0.png'], self.resources[candy_num_list[2]+'candy'+candy_type+'-0.png'],self.resources["buttonpress.wav"], 100, self)]
        self.buttons = self.buttons + [RespButton(self.test_locations[3][0], self.test_locations[3][1], 150, 115, candy_num_list[3], self.resources[candy_num_list[3]+'candy'+candy_type+'-0.png'], self.resources[candy_num_list[3]+'candy'+candy_type+'-0.png'],self.resources["buttonpress.wav"], 100, self)]
        self.buttons = self.buttons + [RespButton(self.test_locations[4][0], self.test_locations[4][1], 150, 115, candy_num_list[4], self.resources[candy_num_list[4]+'candy'+candy_type+'-0.png'], self.resources[candy_num_list[4]+'candy'+candy_type+'-0.png'],self.resources["buttonpress.wav"], 100, self)]
        
        for i in self.buttons:
            i.draw(mysurf)
    
    #------------------------------------------------------------
    # redraw_buttons
    #------------------------------------------------------------
    def redraw_buttons(self, mysurf, loc=None):
        for i in self.buttons:
            i.draw(mysurf)
    
    #------------------------------------------------------------
    # get_click_response
    #------------------------------------------------------------
    def get_click_response(self):
        exit = False;
        while not exit:
            for event in pygame.event.get():
                if event.type == QUIT:
                    self.on_exit()
                elif event.type == KEYDOWN:
                    if pygame.key.get_pressed()[K_LSHIFT] and pygame.key.get_pressed()[K_BACKQUOTE]:
                        self.on_exit()
                elif event.type == MOUSEBUTTONDOWN:
                    #print("here")
                    (x,y) = pygame.mouse.get_pos()
                    for but in self.buttons:
                        if (but.containsPoint(x, y)):
                            rescode = but.do(self.screen)
                            exit = True
        return rescode
    
    #------------------------------------------------------------
    # get_click_response_and_rt
    #------------------------------------------------------------
    def get_click_response_and_rt(self):
        time_stamp = pygame.time.get_ticks()
        res = self.get_click_response()
        
        rt = pygame.time.get_ticks() - time_stamp
        return [res, rt]
    
    #------------------------------------------------------------
    # get_payoff
    #------------------------------------------------------------
    def get_payoff(self, res):

        payoff = self.rewards[res]
                
        return payoff
        
        
    #------------------------------------------------------------
    # update_point_meter
    # this draws and updates the total point meter on the side of the screen
    #------------------------------------------------------------
    def update_point_meter(self, background):
    
        # draw the frame
        pygame.draw.rect(background, white, pygame.Rect(865, 160, 90, 480), 3)

        # draw the blue bar with current total points
        total_points_in_bar = 1000
        total_points = self.earnings
        pixels_per_point = 480./total_points_in_bar
        bar_height = total_points*pixels_per_point
        bar_y = 160 + (total_points_in_bar*pixels_per_point - bar_height)
        pygame.draw.rect(background, blue, pygame.Rect(868, bar_y, 84, bar_height))
        
        # draw the white lines indicating the goals
        goal_line_Ys = [256, 352, 448, 544]
        for y in goal_line_Ys:
            
            pygame.draw.line(background, white, (865,y), (955,y), 3)
            
        #self.resources["correct.wav"].play()

    #------------------------------------------------------------
    # do_trial:
    # this controls what happens on each trial of the main task
    #------------------------------------------------------------
    def do_trial(self, trialnum, countdown):
        
        pygame.mouse.set_visible(1)
        
        # set up the screen, buttons, meter etc
        background = self.clear_screen(black)
        self.draw_buttons(background)
        self.update_point_meter(background)
        self.update_display(background)
        
        # wait for and collect response
        [res, rt] = self.get_click_response_and_rt()
        #pygame.mouse.set_visible(0)
        
        payoff = int(self.get_payoff(res))
        self.earnings += payoff

        # small delay between reward presentation and updating point meter
        pygame.time.wait(250)
        self.update_point_meter(background)
        self.update_display(background) 
        
        #pygame.time.wait(100) 
        
        # reset buttons
        self.redraw_buttons(background)
        self.update_display(background)
        
        # output results of trial to datafile
        # -key to columns:
        # 		subject number
        #		phase of experiment
        #		trial number
        #		condition number
        #		response
        #		response time
        #		reward received
        #		total points earned
        #		creature image chosen
        #		list of reward locations in this order: top-left, bottom-left, top-right, bottom-right
        self.output_trial([self.subj, 'main', self.trial, self.cond, res, rt, payoff, self.earnings, self.creatures[int(res)], self.rewards])
        
        self.trial+=1
        
        # short intertrial interval
        self.escapable_sleep(250)
        pygame.mouse.set_visible(1)
        
        
        
    #------------------------------------------------------------
    # do_test_trial
    # this controls what happens in the final text trials at the end
    #------------------------------------------------------------
    def do_test_trial(self, trialnum, countdown, creature_index, candy_type):
        
        pygame.mouse.set_visible(1)
        background = self.clear_screen(black)
        
        # display the creature
        self.show_image_add(background, self.creatures[int(creature_index)], 0, -135)
        
        # display the response options
        self.draw_test_buttons(background, candy_type)
        
        # Display text
        img_text = "If you picked this creature, how much candy do you think you'd get?"
        self.place_text_image(background, img_text, 36, 0, 75, white, black)
            
        self.update_display(background)
        
        # wait for and get the response
        [res, rt] = self.get_click_response_and_rt()
        #pygame.mouse.set_visible(0)
                
        pygame.time.wait(100) 
        
        background = self.clear_screen(black)
        self.update_display(background)
        
        # output results of trial to datafile
        # -key to columns:
        # 		subject number
        #		phase of experiment
        #		trial number
        #		condition number
        #		response
        #		response time
        #		reward received :(i.e. none for this phase)
        #		total points earned
        #		creature image displayed
        #		list of reward locations in this order: top-left, bottom-left, top-right, bottom-right
        self.output_trial([self.subj, 'test', self.trial, self.cond, res, rt, 'NA', self.earnings, self.creatures[int(creature_index)], self.rewards])
        
        self.trial+=1
        
        # short ITI
        self.escapable_sleep(500)
        pygame.mouse.set_visible(1)
    
    #------------------------------------------------------------
    # do_regular_exp
    # this coordinates the experiment as a whole
    #------------------------------------------------------------
    def do_regular_exp(self):

        
        stage = 1
   
        while(stage!=3):
            if stage == 1:
                self.show_instructions('instructions-adults.png','next.gif', None)
                stage = 2
            elif stage == 2:
                if self.show_instructions('instructions.png','begin.gif', 'back.gif')==BACK:
                    stage = 1
                else:
                    stage = 3
        
        # set up the screen in preparation for the task
        background = self.show_centered_image('instructions-background.gif', black)
        
        
        
        
        self.draw_buttons(background)
        self.update_display(background)

        # do trials of the main task
        for i in range(1,ntrials+1):
            
            self.do_trial(i,-1)
            
            # breaks for stickers when a goal is reached
            if self.earnings > (self.goal_reached+1)*goal_every:
            
                self.goal_reached += 1
                
                # play the sound and display the screen
                self.resources["celebrate.wav"].play()
                self.show_instructions('break.png', 'next.gif', None)
                
                # reset the screen
                background = self.show_centered_image('instructions-background.gif',black)
                self.update_display(background)

                pygame.time.wait(1000)
        
        self.show_instructions('instructions-test.png', 'begin.gif', None)

        # prepare for test phase
        candy_types = ['A', 'B', 'C', 'D']
        
        # randomize order of candy color presentation
        shuffle(candy_types)
        
        # randomize order of creature presentation
        creature_indexes = [0, 1, 2, 3]
        shuffle(creature_indexes)
        
        # do each of the test trials
        for type in candy_types:
            for creature in creature_indexes:
                self.do_test_trial( self.trial, -1, creature, type)
        
        # display the final screen
        self.final_screen()

#-------------------------------------------------------------
# main                   
#-------------------------------------------------------------
def main():
    global laptop, experimentname;
    experiment = ExplDevExp(laptop, screenres, experimentname)
    experiment.do_regular_exp()

#------------------------------------------------------------
# let's start
#------------------------------------------------------------
if __name__ == '__main__':
    main()
