#!/bin/bash
erl -compile nodes
erl -noshell -run nodes test_abort -noshell
