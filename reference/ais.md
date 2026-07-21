# Example AIS dataset

A subset of Automatic Identification System (AIS) messages collected in
the North Sea on 1 November 2022. The dataset contains vessel positions
together with static and dynamic AIS information, including vessel
identity, navigational status, speed, heading, dimensions, draught and
destination.

## Format

A data frame with AIS messages for multiple vessels. The main variables
include:

- datetime:

  Date and time of the AIS message (UTC).

- mmsi:

  Anonymized Maritime Mobile Service Identity (MMSI) of the vessel.

- lon:

  Longitude (decimal degrees, WGS84).

- lat:

  Latitude (decimal degrees, WGS84).

- navigational_status:

  Reported navigational status.

- SOG:

  Speed over ground (knots).

- Heading:

  True heading (degrees).

- imo:

  Reported International Maritime Organization (IMO) number.

- shiptype:

  Reported vessel type.

- length:

  Reported vessel length (m).

- name:

  Reported vessel name.

- width:

  Reported vessel width (m).

- draught:

  Reported vessel draught (m).

## Source

Example subset extracted from AIS observations.

## Details

The dataset is intended for demonstrating the main functions of
AISanalyze, including travel distance estimation, GPS error correction,
vessel characteristic estimation, interpolation and vessel extraction.
Original MMSI identifiers have been replaced with anonymized values to
protect vessel confidentiality.
