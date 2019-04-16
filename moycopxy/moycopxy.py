#!/usr/bin/python

# Pass files a arguments to the script.
# eg :  python3 moy.py data/*csv

import sys
import os
import csv
import copy

files = sys.argv[1::]

# global (averages) stats file
global_stats_file = "global_stats.csv"

# write individual files in subdirectory
out_dir = "out"

if (len(files) == 0):
    print ("Missing input files as arguments")
    sys.exit(1)

try:
    os.mkdir(out_dir)
except:
    pass

global_stats = {}

# build headers to be written on first line, according to data
def headers(stats_data) :
    headers = []
    first_time = next(iter(stats_data))
    for emotion in sorted(stats_data[first_time].keys()):
        for dist in sorted(stats_data[first_time][emotion].keys()):
            headers.extend(["time",
                            emotion + "_" + dist + "_X",
                            emotion + "_" + dist + "_Y"])
    return headers

# compute average of a list of floats
def mean(lst):
    return sum(lst) / len(lst)

# "Initialize empty parts of the stats tree"
def init_dict(dict, time, emot, dist, XY):
    if (dict.get(time, None) == None):
        dict[time] = {}
    if(dict[time].get(emot, None) == None):
        dict[time][emot] = {}
    if(dict[time][emot].get(dist, None) == None):
        dict[time][emot][dist] = {}
    if(dict[time][emot][dist].get('X', None) == None):
        dict[time][emot][dist]['X'] = copy.copy(XY)
        dict[time][emot][dist]['Y'] = copy.copy(XY)
    return dict

##
# Iterate on all the files passed as argulents
for in_file in files:
    with open(in_file, 'r') as in_fd:
        # Create a CSV reader from the file
        in_csv = csv.reader(in_fd, delimiter=',', quotechar='"')
        stats = {};
        for row in in_csv:
            if (row[0] != "condition_emot"):
                emot = row[0]
                dist = row[1]
                time = float(row[2])
                Y = float(row[3])
                X = float(row[4])

                # we create individual files and global stats in the same pass

                # First initialize unexisting parts of the nested dict
                # as Python has no mecanism to do this by itself
                init_dict(global_stats, time, emot, dist, [])
                init_dict(stats, time, emot, dist, {})

                # Add values to the global stats
                global_stats[time][emot][dist]['X'].append(X)
                global_stats[time][emot][dist]['Y'].append(Y)

                # Set individual stats
                stats[time][emot][dist]['X'] = X
                stats[time][emot][dist]['Y'] = Y

    # Individual file path, should work on both Unix & Windows
    out_file = os.path.join(out_dir, os.path.basename(in_file))

    print ("* Writing subject stats : " + out_file)
    with open(out_file, 'w') as out_fd:
        out_csv = csv.writer(out_fd)
        # Set up a csv writer for individual stats
        out_csv.writerow(headers(stats))

        # Read stats, per timestamps,
        for time, dict_time in sorted(stats.items()):
            row = []
            for emotion,dict_emotion in sorted(dict_time.items()):
                for dist,dict_dist in sorted(dict_emotion.items()):
                    X_value = dict_dist['X']
                    Y_value = dict_dist['Y']
                    # and extend row with emotion/dist values
                    row.extend([time, X_value, Y_value])
            out_csv.writerow(row)

print
print ("* Writing global stats : " + global_stats_file)
with open(global_stats_file, 'w') as out_fd:
    out_csv = csv.writer(out_fd)
    out_csv.writerow(headers(global_stats))

    for time, time_dict in sorted(global_stats.items()):
        row = []
        for emotion, emotion_dict in sorted(time_dict.items()):
            for dist, dist_dict in sorted(emotion_dict.items()):
                # Same as individuals, but compute average of values for each
                # X & Y
                mean_X = mean(dist_dict['X'])
                mean_Y = mean(dist_dict['Y'])
                row.extend([time, mean_X, mean_Y])
        out_csv.writerow(row)

print ("All done !")
