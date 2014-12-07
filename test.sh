#!/bin/bash
erl -compile nodes
erl -noshell -run nodes setup -run nodes start
