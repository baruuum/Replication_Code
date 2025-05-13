#!/usr/bin/env snakemake -s

from shutil import rmtree

configfile: "config.yaml"

onsuccess:
    rmtree(".snakemake")
    rmtree(log_dir + "sim/aggregate")
    rmtree(log_dir + "sim/tmpfits")



## ------------------------------------------------------------------
## Paths
## ------------------------------------------------------------------

res_dir   = config["paths"]["res_dir"]
table_dir = config["paths"]["table_dir"]
plot_dir  = config["paths"]["plot_dir"] 
fit_dir   = config["paths"]["fit_dir"]
sim_dir   = config["paths"]["sim_dir"]
log_dir   = config["paths"]["log_dir"]



## ------------------------------------------------------------------
## Final output files
## ------------------------------------------------------------------

# Examples
tab_psi_homophily = table_dir   + "example_psi_homophily.tex"
plot_homophily    = plot_dir    + "example_homophily.pdf"
tab_psi_hierarchy = table_dir   + "example_psi_hierarchy.tex"
plot_hierarchy    = plot_dir    + "example_hierarchy.pdf"
tab_psi_exchange  = table_dir   + "example_psi_exchange.tex"
plot_exchange     = plot_dir    + "example_exchange.pdf"
tab_psi_cyclical  = table_dir   + "example_psi_cyclical.tex"
plot_cyclical     = plot_dir    + "example_cyclical.pdf"


# Breiger (1981) analysis
vem_fit_breiger         = fit_dir    + "vem_fit_breiger.rds"
plot_icl_breiger        = plot_dir   + "icl_breiger.pdf"
plot_heat_breiger       = plot_dir   + "heat_breiger.pdf"
plot_comp_breiger       = plot_dir   + "breiger_vem_comparison.pdf"
tab_icl_breiger         = table_dir  + "breiger_icl.tex"
tab_bic_comp_breiger     = table_dir  + "breiger_vem_BIC_comparison.tex"
tab_crosstab_breiger    = table_dir  + "breiger_crosstab.tex"
collapsed_vem_breiger   = table_dir  + "collapsed_vem_breiger.tex"
collapsed_pairs_breiger = table_dir  + "collapsed_pairs_breiger.tex"
clustering_breiger      = table_dir  + "clustering_breiger.tex"


# Goodman (1981) analysis
vem_fit_goodman         = fit_dir   + "vem_fit_goodman.rds"
plot_icl_goodman        = plot_dir  + "icl_goodman.pdf"
plot_heat_goodman       = plot_dir  + "heat_goodman.pdf"
tab_icl_goodman         = table_dir + "goodman_icl.tex"
collapsed_vem_goodman   = table_dir + "collapsed_vem_goodman.tex"
collapsed_pairs_goodman = table_dir + "collapsed_pairs_goodman.tex"
clustering_goodman      = table_dir + "clustering_goodman.tex"

# Simulation
sim_fixed_pars = sim_dir + "sim_fixed_pars.rds"
sim_seed_list  = sim_dir + "sim_seed_list.csv"
sim_res_base = sim_dir + "runs/sim_{run}_{image}_{samp}_{block}_{beta}_{alpha}.rds"
plot_runtime  = plot_dir + "sim_runtime.pdf"
plot_rand     = plot_dir + "sim_rand.pdf"
plot_nmi      = plot_dir + "sim_nmi.pdf"

# final simulation output
sim_res = expand(
        sim_res_base,
        run   = list(range(1, config["sim_n_run"] + 1)),
        image = config["sim_image"],
        samp  = config["sim_sample_sizes"],
        block = config["sim_block_sizes"],
        beta  = config["sim_beta"],
        alpha = config["sim_alpha"]
    )

# update if needed
if config["sim_block10"]:
    sim_res = sim_res + expand(
        sim_res_base,
        run   = list(range(1, config["sim_n_run"] + 1)),
        image = config["sim_image"],
        samp  = 500,
        block = 10,
        beta  = config["sim_beta"],
        alpha = config["sim_alpha"]
    )



## ------------------------------------------------------------------
## Rules
## ------------------------------------------------------------------

rule final_output:
    input:
        # Examples
        tab_psi_homophily,
        plot_homophily,
        tab_psi_hierarchy,
        plot_hierarchy,
        tab_psi_exchange,
        plot_exchange,
        tab_psi_cyclical,
        plot_cyclical,

        # Breiger
        vem_fit_breiger,
        plot_icl_breiger,
        plot_heat_breiger,
        plot_comp_breiger,
        tab_icl_breiger,
        tab_bic_comp_breiger,
        tab_crosstab_breiger,
        collapsed_vem_breiger,
        collapsed_pairs_breiger,
        clustering_breiger,

        # Goodman
        vem_fit_goodman,
        collapsed_vem_goodman,
        collapsed_pairs_goodman,
        clustering_goodman,
        tab_icl_goodman,

        # Simulation
        sim_fixed_pars,
        sim_seed_list,
        sim_res,
        plot_runtime,
        plot_rand,

        # MISC
        log_dir + ".loglin_run_finished",
        log_dir + "sim/aggregate_logs.tar.gz",
        log_dir + "sim/refit_logs.tar.gz"


rule hypothetical_examples:
    output:
        tab_psi_homophily = tab_psi_homophily,
        plot_homophily = plot_homophily,
        tab_psi_hierarchy = tab_psi_hierarchy,
        plot_hierarchy = plot_hierarchy,
        tab_psi_exchange = tab_psi_exchange,
        plot_exchange = plot_exchange,
        tab_psi_cyclical = tab_psi_cyclical,
        plot_cyclical = plot_cyclical
    params:
        seed = config["seed_examples"]
    log: log_dir + "hypothetical_examples.log"
    threads: 1
    script: "scripts/examples.R"

rule run_loglin_time:
    output:
        flag = log_dir + ".loglin_run_finished"
    log: log_dir + "run_loglin_time.log"
    threads: 1
    script: "scripts/loglin_block_time.R"

# Analysis of Breiger (1981)

rule vem_fit_breiger:
    output:
        fitted_models = vem_fit_breiger
    params:
        seed = config["seed_breiger"],
        n_refit = config["breiger_n_refit"],
        max_blocks = config["breiger_max_blocks"]
    log: log_dir + "vem_fit_breiger.log"
    threads: min(config["breiger_n_refit"] + 1, workflow.cores)
    script: "scripts/vem_fit_breiger.R"

rule analyze_breiger:
    params:
        seed_breiger_community = config["seed_breiger_community"],
        clust_algs = config["test_clust_algs"]
    input:
        fitted_models = rules.vem_fit_breiger.output.fitted_models
    output:
        plot_icl_breiger = plot_icl_breiger,
        plot_heat_breiger = plot_heat_breiger,
        plot_comp_breiger = plot_comp_breiger,
        tab_icl_breiger = tab_icl_breiger,
        tab_bic_comp_breiger = tab_bic_comp_breiger,
        tab_crosstab_breiger = tab_crosstab_breiger,
        collapsed_vem = collapsed_vem_breiger,
        collapsed_pairs = collapsed_pairs_breiger,
        clustering = clustering_breiger
    log: log_dir + "vem_analyze_breiger.log"
    threads: 1
    script: "scripts/vem_analyze_breiger.R"

# Analysis of Goodman (1981)

rule vem_fit_goodman:
    output:
        fitted_models = vem_fit_goodman
    params:
        seed = config["seed_goodman"],
        n_refit = config["goodman_n_refit"],
        max_blocks = config["goodman_max_blocks"]
    log: log_dir + "vem_fit_goodman.log"
    threads: min(config["goodman_n_refit"] + 1, workflow.cores)
    script: "scripts/vem_fit_goodman.R"

rule analyze_goodman:
    params:
        seed_goodman_community = config["seed_goodman_community"],
        clust_algs = config["test_clust_algs"]
    input:
        fitted_models = rules.vem_fit_goodman.output.fitted_models
    output:
        plot_icl_goodman = plot_icl_goodman,
        plot_heat_goodman = plot_heat_goodman,
        tab_icl_goodman = tab_icl_goodman,
        collapsed_vem = collapsed_vem_goodman,
        collapsed_pairs = collapsed_pairs_goodman,
        clustering = clustering_goodman
    log: log_dir + "vem_analyze_goodman.log"
    threads: 1
    script: "scripts/vem_analyze_goodman.R"

# Simulation experiments
rule make_sim_fixed_pars:
    output:
        sim_fixed_pars = sim_fixed_pars,
        sim_seed_list = sim_seed_list
    params:
        seed         = config["seed_sim"],
        run          = list(range(1, config["sim_n_run"] + 1)),
        image        = config["sim_image"],
        sample_sizes = config["sim_sample_sizes"],
        block_sizes  = config["sim_block_sizes"],
        beta         = config["sim_beta"],
        alpha        = config["sim_alpha"],
        block10      = config["sim_block10"]
    log: log_dir + "make_sim_fixed_pars.log"
    threads: 1
    script: "scripts/sim_make_fixed_pars.R"

rule run_sims:
    input:
        sim_fixed_pars = rules.make_sim_fixed_pars.output.sim_fixed_pars,
        sim_seed_list = rules.make_sim_fixed_pars.output.sim_seed_list
    output: 
        temp(res_dir + "tmpfits/sim_{run}_{image}_{samp}_{block}_{beta}_{alpha}_{refit}.rds")
    log: log_dir + "sim/tmpfits/sim_{run}_{image}_{samp}_{block}_{beta}_{alpha}_{refit}.log"
    threads: 1
    script: "scripts/sim_run.R"

rule aggregate_sims:
    input:
        sim_refits = expand(
            rules.run_sims.output,
            refit = list(range(1, config["sim_n_refit"] + 1)),
            allow_missing = True)
    output: 
        res = sim_res_base
    log: log_dir + "sim/aggregate/sim_{run}_{image}_{samp}_{block}_{beta}_{alpha}.log"
    threads: 1
    script: "scripts/aggregate_sim_refits.R"

rule compress_sim_aggregate_logs:
    input: sim_res
    output: log_dir + "sim/aggregate_logs.tar.gz"
    shell:
        """
            tar -zcf {output} $(dirname {output})/aggregate
        """

rule compress_sim_refit_logs:
    input: sim_res
    output: log_dir + "sim/refit_logs.tar.gz"
    shell:
        """
            tar -zcf {output} $(dirname {output})/tmpfits
        """

rule analyze_sims:
    input:
        res = sim_res
    output:
        plot_runtime = plot_runtime,
        plot_rand = plot_rand,
        plot_nmi = plot_nmi
    threads: min(4, workflow.cores)
    log: log_dir + "analyze_sims.log"
    script: "scripts/sim_analysis.R"

rule create_gray_figs:
    input: 
        plot_homophily      = plot_homophily,
        plot_hierarchy      = plot_hierarchy,
        plot_exchange       = plot_exchange,
        plot_cyclical       = plot_cyclical,
        plot_icl_breiger    = plot_icl_breiger,
        plot_heat_breiger   = plot_heat_breiger,
        plot_comp_breiger   = plot_comp_breiger,
        plot_runtime        = plot_runtime,
        plot_rand           = plot_rand
    output:
        gray_fig_flag = log_dir + ".gray_figs_done"
    threads: 1
    shell:
        "bash make_gfigs.sh"

### EOF ###