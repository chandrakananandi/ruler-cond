import json
import sys
import matplotlib as mpl
import matplotlib.pyplot as plt

mpl.rcParams["savefig.directory"] = ""
current_plotted = []

def get_dataset(filepath, benchmark_name=None):

    fp = open(filepath, "r")
    # fp = open("scored-disc.json","r")
    all_patterns = json.load(fp)
    cost_accuracies = [{"benchmark": x['name'], "best": x['output'], "data": x['cost-accuracy']} for x in all_patterns['tests']]
    
    # get merged data
    cost_accuracies_merged = all_patterns.get('frontier', [])

    # accumulate per-benchmark data
    cost_accuracies_per_benchmark = []
    for benchmark in cost_accuracies:
        # WHYYY HERBIE ?!?!?!
        # start : (cost, error)
        # best : (cost, error)
        # others : ((cost, error, expr), ...)
        if len(benchmark['data']) == 0:
            cost_accuracies_per_benchmark.append({"benchmark": benchmark["benchmark"], "data": []})
        else:
            _, best, others = benchmark['data']

            # add best to frontier
            pts = [{"cost": best[0], "error": best[1], "expr": benchmark["best"]}]
            # add rest to frontier
            pts += ([{"cost": x[0], "error": x[1], "expr": x[2]} for x in others])

            # all done
            cost_accuracies_per_benchmark.append({"benchmark": benchmark["benchmark"], "data": pts})
    
    # print(cost_accuracies_per_benchmark)
    # print(cost_accuracies_merged)

    if benchmark_name is None:
        current_plotted = [{'expr' : (x, y)} for x, y in cost_accuracies_merged]
        xs = [x for x, _ in cost_accuracies_merged]
        ys = [y for _, y in cost_accuracies_merged]
    else:
        current_plotted = next(b for b in cost_accuracies_per_benchmark if b["benchmark"] == benchmark_name)["data"]
        xs = [x["cost"] for x in current_plotted]
        ys = [y["error"] for y in current_plotted]

    return (xs, ys, current_plotted)

fig, ax = plt.subplots()

herbie_path = sys.argv[1]
slide_rule_path = sys.argv[2]
oopsla21_path = sys.argv[3]
output_file = sys.argv[4]

first_input =  ('SlideRule', '{}/result.json'.format(herbie_path))
second_input = ('Ruler',     '{}/result.json'.format(slide_rule_path))
third_input =  ('Herbie',    '{}/result.json'.format(oopsla21_path))

xs1, ys1, first_plotted =  get_dataset(first_input[1])
xs2, ys2, second_plotted = get_dataset(second_input[1])
xs3, ys3, third_plotted =  get_dataset(third_input[1])

plt.plot(xs1, ys1)
plt.plot(xs2, ys2, color="red")
plt.plot(xs3, ys3, color="green")

ax.legend([first_input[0], second_input[0], third_input[0]])
annot = ax.annotate("", xy=(0,0), xytext=(20,20),textcoords="offset points",
                    bbox=dict(boxstyle="round", fc="w"),
                    arrowprops=dict(arrowstyle="->"))
annot.set_visible(False)

def update_annot(ind, scatter, points):
    pos = scatter.get_offsets()[ind["ind"][0]]
    annot.xy = pos
    # print(ind)
    # ind is all indices under that point
    # print(cost_accuracies_per_benchmark[ind["ind"][0]])
    first = points[ind["ind"][0]]["expr"]
    text = f"{first}"
    annot.set_text(text)
    annot.get_bbox_patch().set_alpha(0.4)

plt.xlabel("Sum of Cost Estimates")
plt.ylabel("Sum of log2(Error)")
plt.savefig(output_file)
