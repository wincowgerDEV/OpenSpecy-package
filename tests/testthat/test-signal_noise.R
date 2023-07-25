
# Load your Open Specy object 
data("raman_hdpe")

test_that("signal_noise returns correct values", {
    # Test 'signal' calculation
    expect_true(round(signal_noise(raman_hdpe, return = "signal"), 2) == 101.17)
    
    # Test 'noise' calculation
    expect_true(round(signal_noise(raman_hdpe, return = "noise"), 2) == 61.01)
    
    # Test 'signal_times_noise' calculation
    expect_true(round(signal_noise(raman_hdpe, return = "signal_times_noise"), 2) == 6172.1)
    
    # Test 'signal_over_noise' calculation
    expect_true(round(signal_noise(raman_hdpe, return = "signal_over_noise"), 2) == 1.66)
    
    # Test 'total_signal' calculation
    expect_true(round(signal_noise(raman_hdpe, return = "total_signal"), 2) == 97527)
})

