#ifndef LINUXDEF_H
#define LINUXDEF_H

#include "PacketLinuxConfig.h"

#ifdef HAVE_LINUX_IF_ETHER_H
#include <linux/if_ether.h>
#endif
#ifdef HAVE_LINUX_IF_PACKET_H
#include <linux/if_packet.h>
#endif

#endif
