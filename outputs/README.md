# Dashboard data

The live dashboard uses a lightweight dataset derived from the Sustainable Energy Authority of Ireland (SEAI) Behavioural Energy and Travel Tracker (BETT).

The underlying survey-derived dataset is **not redistributed in this public repository**. This is intentional while redistribution permissions and licensing conditions are respected.

To run the app locally with authorised access, place the prepared file below in this folder:

```text
bett_dashboard_small.csv
```

The application expects the following fields:

- `year`
- `season`
- `wave_label_ordered`
- `income_group`
- `tenure`
- `region`
- `urbanrural`
- `bill_difficulty_binary`
- `bills_worry_binary`
- `heating_7days_binary`
- `drive2k_binary`

The raw BETT dataset and the larger cleaned analytical dataset are not included.
