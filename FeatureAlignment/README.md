## Motivation: Why is Our Project Important?
Compared to Horizontal Federated Learning, Vertical Federated Learning is a newly emerging topic. There is limited Research on this topic.

Like mentioned before, Feature Alignment is the first step for Vertical Federated Learning, but there’s no available package that can do this job. Most packages, like Recordlinkage, and Dedupe are only focused on Entity Resolution, and can only have good preference when the data type is “String”. Furthermore, they also place complex requirements on datasets, such as requiring unique identifiers.

However, in the real world, there are two main challenges. First, the data is highly heterogeneous (It’s a collaborative learning setup!). Second, companies (data providers) are under restriction on to what extent they can share their data (the dataset they provide is encrypted or sampled).

Therefore, we’d like to develop a package useful on Feature Alignment issues, especially for numeric matching. In this package, we can run our algorithm without knowing the feature name and we only have partial (sampled) data.

## Problem Set: Feature Alignment

This is a package to deal with Privacy Level 2 and 3 problems. In this case, either we have the
same feature sample, but in a different order; or we have very different feature samples, and we
need to find the overlapping parts.

When a dataset is vertically partitioned across organisations the problem arises of how to
identify corresponding entities. The ideal solution would be joining datasets by common unique
IDs; however, across organisations this is rarely feasible. In a non-privacy-preserving context,
matching can be based on shared personal identifiers, such as name, address, gender, and date
of birth, that are effectively used as weak IDs. However In our setting, weak identifiers are
considered private to each party so that not included in the dataset.
As I mentioned before, the 2 most important steps in entity resolution are blocking and
comprising. Since feature alignment is a horizontal version of entity resolution, we can also
adapt the idea to develop our method.

According to our experience, previous research, such as Dedupe, RecordLinkage, and other
packages only support entity resolution instead of feature alignment. And their work has good
results in string matching, but low performance in numeric matching issues, or even does not
support numeric matching at all. In light of that, I think it would be a good idea to develop an
R package dedicated to feature alignment, especially for numeric matching.
