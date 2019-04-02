{-# LANGUAGE OverloadedStrings #-}

module Std.IO.UDPSpec where

import           Control.Concurrent
import           Control.Monad
import           Data.Bits
import           Std.Data.Vector         as V
import           Std.Data.Vector.Base    as V
import           Data.List               as List
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Std.IO.Exception
import           Std.IO.UDP
import           Std.IO.Resource
import           Std.IO.SockAddr
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = describe "UDP operations" $ do
    it "roundtrip test" $ do
        let testMsg = V.replicate 256 48
            longMsg = V.replicate 2048 48
            addr = SockAddrInet 12345 inetLoopback
        withResource (initUDP defaultUDPConfig{sendMsgSize = 2048}) $ \ c ->
            withResource (initUDP defaultUDPConfig{localUDPAddr = Just (addr,UV_UDP_DEFAULT)}) $ \ s -> do
                forkIO $ sendUDP c addr testMsg
                [(_, partial, rcvMsg)]<- recvUDP s
                partial @=? False
                rcvMsg @=? testMsg

                threadDelay 100000

                forkIO $ sendUDP c addr longMsg
                [(_, partial, rcvMsg)]<- recvUDP s
                partial @=? True

    it "UDP sending addr test" $ do
        let testMsg = V.replicate 256 48
            addr = SockAddrInet 12346 inetLoopback
            addr' = SockAddrInet 12347 inetLoopback
        withResource (initUDP defaultUDPConfig{localUDPAddr = Just (addr,UV_UDP_DEFAULT)}) $ \ c ->
            withResource (initUDP defaultUDPConfig{localUDPAddr = Just (addr',UV_UDP_DEFAULT)}) $ \ s -> do
                forkIO $ sendUDP c addr' testMsg
                [(rcvAddr, _, _)]<- recvUDP s
                Just addr @=? rcvAddr

    it "overlong message exception" $ do
        let testMsg = V.replicate 4096 48
            addr = SockAddrInet 12348 inetLoopback
        withResource (initUDP defaultUDPConfig) $ \ c ->
            withResource (initUDP defaultUDPConfig) $ \ s -> do
                sendUDP c addr testMsg `shouldThrow` anyException

    {- This test need a local broadcast address, so it's disabled by default.
    it "UDP sending addr test" $ do
        let testMsg = V.replicate 256 48
            addr = SockAddrInet 12349 (tupleToInetAddr (10,92,239,187))
            addr' = SockAddrInet 12350 inetAny
            broadcastAddr = SockAddrInet 12350 (tupleToInetAddr (10,92,239,255))
        withResource (initUDP defaultUDPConfig{localUDPAddr = Just (addr,UV_UDP_DEFAULT)}) $ \ c ->
            withResource (initUDP defaultUDPConfig{localUDPAddr = Just (addr',UV_UDP_DEFAULT)}) $ \ s -> do
                    setBroadcast c True
                    forkIO $ sendUDP c broadcastAddr testMsg
                    [(rcvAddr, _, rcvMsg)]<- recvUDP s
                    Just addr @=? rcvAddr
                    rcvMsg @=? testMsg
    -}
