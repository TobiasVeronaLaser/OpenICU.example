# Stages
- PreMeds: extraction step
1. Sharding: Partition into smaller files
2. Split and shard subject: split into training, tuning and held_out, and shared into theses groups
3. Convert to Sharded events: meds
4. Merge to MEDS cohort: combines the sharded event tables into single cohort structure (still split into training-, ... -files)
5. Extract code metadata: Metadata
6. Finalue MEDS metadata: unified Metadata for split SAme files in 5. (?)
7. Finalize MEDS data: Same files in 4. (?)


# Steps betwen 3., 4. and 5.
- Ram efficency

