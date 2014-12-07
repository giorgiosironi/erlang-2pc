#!/bin/bash
erl -compile nodes
erl -noshell -run nodes start -noshell
