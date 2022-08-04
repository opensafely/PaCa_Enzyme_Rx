# Pancreatic enzyme supplementation in people with pancreatic cancer

This is the code and configuration for a cohort study undertaken using OpenSAFELY-TPP for investigating the effect of COVID-19 on prescribing of pancreatic enzyme replacement therapy (PERT) in people diagnosed with unresectable pancreatic cancer. 

An accompanying pre-print: [<i>The impact of COVID-19 on prescribing of pancreatic enzyme replacement therapy for people with unresectable pancreatic cancer in England. A cohort study using OpenSafely-TPP</i>](https://www.medrxiv.org/content/10.1101/2022.07.08.22277317v1) is available on MedRxiv.

You can run this project via [Gitpod](https://gitpod.io) in a web browser by clicking on this badge: [![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-908a85?logo=gitpod)](https://gitpod.io/#https://github.com/opensafely/PaCa_Enzyme_Rx)

## Outputs 
* The preprint of the paper is [here](https://www.medrxiv.org/content/10.1101/2022.07.08.22277317v1)
* Raw model outputs, including charts, crosstabs, etc, are in `released_outputs/`
* If you are interested in how we defined our variables, take a look at the [study definition](analysis/study_definition.py); this is written in `python`, but non-programmers should be able to understand what is going on there
* If you are interested in how we defined our code lists, look in the [codelists folder](./codelists/).
* Developers and epidemiologists interested in the framework should review [the OpenSAFELY documentation](https://docs.opensafely.org)

# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).

# Licences
As standard, research projects have a MIT license. 
