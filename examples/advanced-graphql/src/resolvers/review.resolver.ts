import { Resolver, Query, Mutation, Arg, Ctx, ID, Info, Int, FieldResolver, Root } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Review, ReviewCreateInput, ReviewUpdateInput, ReviewQueryArgs, ReviewConnection, ReviewFilterInput, ReviewSortInput, Product } from '../../schema'
import { ConnectionBuilder } from '../../schema-helpers'
import type { Context } from './types'

@Resolver(() => Review)
export class ReviewResolver {
	@Query(() => Review, { nullable: true })
	async review(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<Review | null> {
		return (await ctx.prisma.review.findUnique({ where: { id } })) as Review | null
	}

	@Query(() => ReviewConnection)
	async reviews(
		@Arg('filter', () => ReviewFilterInput, { nullable: true }) filter: ReviewFilterInput | undefined,
		@Arg('sort', () => ReviewSortInput, { nullable: true }) sort: ReviewSortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() ctx: Context,
	): Promise<ReviewConnection> {
		const args: ReviewQueryArgs = { filter, sort, first, after, last, before }
		const config = ConnectionBuilder.buildReviewConnectionConfig(args, info)

		const items = await ctx.prisma.review.findMany(config.findManyOptions)
		const totalCount = await ctx.prisma.review.count(config.countOptions)

		return ConnectionBuilder.processResults(items, totalCount, config.paginationInfo) as ReviewConnection
	}

	@Mutation(() => Review)
	async createReview(@Arg('input', () => ReviewCreateInput) input: ReviewCreateInput, @Ctx() ctx: Context): Promise<Review> {
		return (await ctx.prisma.review.create({ data: input })) as Review
	}

	@Mutation(() => Review)
	async updateReview(@Arg('id', () => ID) id: string, @Arg('input', () => ReviewUpdateInput) input: ReviewUpdateInput, @Ctx() ctx: Context): Promise<Review> {
		return (await ctx.prisma.review.update({ where: { id }, data: input })) as Review
	}

	@Mutation(() => Boolean)
	async deleteReview(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.review.delete({ where: { id } })
		return true
	}

	@FieldResolver(() => Product, { nullable: true })
	async product(@Root() review: Review, @Ctx() ctx: Context): Promise<Product | null> {
		return (await ctx.prisma.product.findUnique({ where: { id: review.productId } })) as Product | null
	}
}
