# mac-back-api

## Summary

This project serves as the control interface to the MacMan game. 
All commands to the game are issued through a websocket which this project provides. 
Much like the original game that is the obvious inspiration the controls and simple and few. 
You can start, restart and tell the player to move in the four compass directions.

After every move you will be provided with the current and previous states of the game.
This information should be used to determine ghost movement and calculate the next player move.

## Getting Started

Follow the instructions in `mac-back-api.core`