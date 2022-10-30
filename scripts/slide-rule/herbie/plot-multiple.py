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

if len(sys.argv) != 10:
    print("Expected 10 arguments")
    sys.exit(1)

fig, axes = plt.subplots(nrows=3, ncols=3, sharex=True, sharey=True, figsize=(10, 10), dpi=120)
axes_list = [item for sublist in axes for item in sublist]

# fig.add_subplot(111, frameon=False)
# plt.tick_params(labelcolor='none', top=False, bottom=False, left=False, right=False)

for i, ax in enumerate(axes_list):
    seed = sys.argv[i + 1]
    herbie_path = 'output/main/{}'.format(seed)
    slide_rule_path = 'output/slide-rule/{}'.format(seed)
    oopsla21_path = 'output/oopsla21/{}'.format(seed)

    xs1, ys1, first_plotted =  get_dataset('{}/results.json'.format(slide_rule_path))
    xs2, ys2, second_plotted = get_dataset('{}/results.json'.format(oopsla21_path))
    xs3, ys3, third_plotted =  get_dataset('{}/results.json'.format(herbie_path))

    ax.plot(xs1, ys1, label="SlideRule")
    ax.plot(xs2, ys2, color="red", label="Ruler")
    ax.plot(xs3, ys3, color="green", label="Herbie")

    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

    annot = ax.annotate("", xy=(0,0), xytext=(20,20),textcoords="offset points",
                        bbox=dict(boxstyle="round", fc="w"),
                        arrowprops=dict(arrowstyle="->"))
    annot.set_visible(False)

handles, labels = axes_list[1].get_legend_handles_labels()
fig.legend(handles, labels, loc='upper right')
fig.supxlabel("Sum of Cost Estimates")
fig.supylabel("Sum of log2(Error)")

plt.tight_layout()
plt.savefig('output/plot/multiples.png', bbox_inches='tight')
