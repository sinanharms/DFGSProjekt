# -*- coding: utf-8 -*-
"""Implements the Atomic-VAEP framework.
Attributes
----------
xfns_default : list(callable)
    The default VAEP features.
"""
from typing import List, Optional

import atomic.spadl as spadlcfg
from vaep.base import VAEP

from . import features as fs
from . import formula as vaep
from . import labels as lab

xfns_default = [
    fs.actiontype,
    fs.actiontype_onehot,
    fs.bodypart,
    fs.bodypart_onehot,
    fs.time,
    fs.team,
    fs.time_delta,
    fs.location,
    fs.polar,
    fs.movement_polar,
    fs.direction,
    fs.goalscore,
]


class AtomicVAEP(VAEP):
    """
    An implementation of the VAEP framework [Decroos19]_ for atomic actions.
    In contrast to the original VAEP framework this extension distinguishes
    the contribution of the player who initiates the action (e.g., gives the
    pass) and the player who completes the action (e.g., receives the pass).
    Parameters
    ----------
    xfns : list
        List of feature transformers (see :mod:`socceraction.atomic.vaep.features`)
        used to describe the game states. Uses :attr:`~socceraction.vaep.base.xfns_default`
        if None.
    nb_prev_actions : int, default=3
        Number of previous actions used to decscribe the game state.
    See Also
    --------
    :class:`socceraction.vaep.VAEP` : Implementation of the original VAEP framework.
    .. [Decroos19] Decroos, Tom, Lotte Bransen, Jan Van Haaren, and Jesse Davis.
        "Actions speak louder than goals: Valuing player actions in soccer." In
        Proceedings of the 25th ACM SIGKDD International Conference on Knowledge
        Discovery & Data Mining, pp. 1851-1861. 2019.
    """

    _spadlcfg = spadlcfg
    _lab = lab
    _fs = fs
    _vaep = vaep

    def __init__(
        self,
        xfns: Optional[List[fs.FeatureTransfomer]] = None,
        nb_prev_actions: int = 3,
    ):
        xfns = xfns_default if xfns is None else xfns
        super().__init__(xfns, nb_prev_actions)
