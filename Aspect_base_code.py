#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jul  7 13:38:04 2018

@author: bosco
"""

import nltk
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')
nltk.download('wordnet')
nltk.download('sentiwordnet')
import ast
from nltk.tokenize import word_tokenize
from nltk.corpus import wordnet
from nltk.corpus import sentiwordnet
from time import gmtime, strftime

def tokenizeReviews():
    tokenizedReviews={}
    inputFile = open("Damy.txt","r").read()
    outputFile=open ("tokens.txt","w")
    tokenizer = nltk.tokenize.punkt.PunktSentenceTokenizer()
    uniqueId=1;

    for sentence in tokenizer.tokenize(inputFile):      
        tokenizedReviews[uniqueId]=sentence
        uniqueId+=1
    outputFile.write(str(tokenizedReviews))
   
    outputFile.close()
    
tokenizeReviews()

def posTagging():
    inputFile = open("tokens.txt","r").read()
    outputFile=open ("tokens_POS.txt","w")
    inputTupples=ast.literal_eval(inputFile)
    outputPost={}
    for key,value in inputTupples.items():
        outputPost[key]=nltk.pos_tag(nltk.word_tokenize(value))
    outputFile.write(str(outputPost))
    outputFile.close()
    
posTagging()

def aspectExtraction():
    inputFile = open("tokens_POS.txt","r").read()
    outputFile=open ("Aspects.txt","w")
    inputTupples=ast.literal_eval(inputFile)
    prevWord=''
    prevTag=''
    currWord=''
    aspectList=[]
    outputDict={}
    #Extracting Aspects
    for key,value in inputTupples.items():
        for word,tag in value:
            if(tag=='NN' or tag=='NNP' or tag=='NNS'):
                if(prevTag=='NN' or prevTag=='NNP'):
                    currWord= prevWord + ' ' + word
                else:
                    aspectList.append(prevWord.upper())
                    currWord= word
            prevWord=currWord
            prevTag=tag
    #Eliminating aspect which has 1 or less count
    for aspect in aspectList:
            if(aspectList.count(aspect)>6):
                    if(outputDict.keys()!=aspect):
                            outputDict[aspect]=aspectList.count(aspect)
    outputAspect=sorted(outputDict.items(), key=lambda x: x[1],reverse = True)
    outputFile.write(str(outputAspect))
    outputFile.close()

aspectExtraction()

def orientation(inputWord): 
    wordSynset=wordnet.synsets(inputWord)
    
    if(len(wordSynset) != 0):
        word=wordSynset[0].name()
      
        orientation=sentiwordnet.senti_synset(word)
        
        if(orientation.pos_score()>orientation.neg_score()):
            return True
        elif(orientation.pos_score()<orientation.neg_score()):
            return False

def identifyOpinionWords():     
    fileName=strftime("%Y-%m-%d %H-%M-%S", gmtime())+'.csv'
    print(fileName)
    inputReviewList = open("tokens_POS.txt","r").read()
    inputAspectList = open("Aspects.txt","r").read()
    outputAspectOpinionList=open ("Final_SA.txt","w")
    inputReviewsTuples=ast.literal_eval(inputReviewList)
    inputAspectTuples=ast.literal_eval(inputAspectList)
    outputAspectOpinionTuples={}
    orientationCache={}
    negativeWordSet = {"don't","never", "nothing", "nowhere", "noone", "none", "not",
                  "hasn't","hadn't","can't","couldn't","shouldn't","won't",
                  "wouldn't","don't","doesn't","didn't","isn't","aren't","ain't"}
    with open(fileName, 'a') as the_file:
                the_file.write('Aspect\tPositive\tNegative\n')
    for aspect,no in inputAspectTuples:
        aspectTokens= word_tokenize(aspect)
        count=0
        for key,value in inputReviewsTuples.items():
            condition=True
            isNegativeSen=False
            for subWord in aspectTokens:
                if(subWord in str(value).upper()):
                    condition = condition and True
                else:
                    condition = condition and False
            if(condition):
                for negWord in negativeWordSet:
                    if(not isNegativeSen):#once senetence is negative no need to check this condition again and again
                        if negWord.upper() in str(value).upper():
                            isNegativeSen=isNegativeSen or True
                outputAspectOpinionTuples.setdefault(aspect,[0,0,0])
                for word,tag in value:
                     if(tag=='JJ' or tag=='JJR' or tag=='JJS'or tag== 'RB' or tag== 'RBR'or tag== 'RBS'):
                         count+=1
                         if(word not in orientationCache):
                             orien=orientation(word)
                             orientationCache[word]=orien
                         else:
                             orien=orientationCache[word]
                         if(isNegativeSen and orien is not None):
                             orien= not orien
                         if(orien==True):
                             outputAspectOpinionTuples[aspect][0]+=1
                         elif(orien==False):
                             outputAspectOpinionTuples[aspect][1]+=1
                         elif(orien is None):
                             outputAspectOpinionTuples[aspect][2]+=1
        if(count>4):
            #print(aspect,' ', outputAspectOpinionTuples[aspect][0], ' ',outputAspectOpinionTuples[aspect][1], ' ',outputAspectOpinionTuples[aspect][2])
            outputAspectOpinionTuples[aspect][0]=round((outputAspectOpinionTuples[aspect][0]/count)*100,2)
            outputAspectOpinionTuples[aspect][1]=round((outputAspectOpinionTuples[aspect][1]/count)*100,2)
            outputAspectOpinionTuples[aspect][2]=round((outputAspectOpinionTuples[aspect][2]/count)*100,2)
            print(aspect,'\t\tPositive => ', outputAspectOpinionTuples[aspect][0], '\tNegative => ',outputAspectOpinionTuples[aspect][1])
            with open(fileName, 'a') as the_file:
                the_file.write(aspect + '\t' + str(outputAspectOpinionTuples[aspect][0])+ '\t'+ str(outputAspectOpinionTuples[aspect][1])+'\n')
                
    outputAspectOpinionList.write(str(outputAspectOpinionTuples))
    outputAspectOpinionList.close();
#-----------------------------------------------------------------------------------
identifyOpinionWords()
