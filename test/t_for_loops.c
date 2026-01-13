void test1 (void)
{
    int i;
    for (i = 0; i < 10; i ++)
        printf("%d\n", i)
    for (int j = 0; j < 10; j ++)
        printf("%d ", j);
    printf("\n");
    for (int k = 0; k <= 5; k ++)
        printf("%d\n", k);
    for (int n = 0; n < 10; n ++)
        for (int m = 0; m <= n; m ++)
            printf("(%d, %d)\n", m, n);
    for (short p = 5; p >= 0; p --)
        printf("%d\n", p);
    /* Should not be a simple loop. */
    for (float q = 0; sin(q) < 0.7; q += 3.14/20.0)
        printf("%f, %f\n", q, sin(q));
}
