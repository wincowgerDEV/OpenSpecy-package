package launcher

import (
	"os"
	"os/signal"
)

func notifyInterrupt(channel chan<- os.Signal) {
	signal.Notify(channel, os.Interrupt)
}

func stopInterrupt(channel chan<- os.Signal) {
	signal.Stop(channel)
}
