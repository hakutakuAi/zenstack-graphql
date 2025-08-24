import { Resolver, Query, Mutation, Arg, Ctx, ID } from 'type-graphql'
import { Tag, TagCreateInput, TagUpdateInput, TagQueryArgs, TagConnection } from '../../schema'
import { Context } from './types'

@Resolver(() => Tag)
export class TagResolver {
	@Query(() => Tag, { nullable: true })
	async tag(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<Tag | null> {
		return ctx.prisma.tag.findUnique({
			where: { id },
			include: {
				products: {
					include: {
						product: true,
					},
				},
			},
		})
	}

	@Query(() => TagConnection)
	async tags(@Arg('args', { nullable: true }) args: TagQueryArgs, @Ctx() ctx: Context): Promise<TagConnection> {
		const take = args?.first || 50
		const skip = args?.after ? 1 : 0
		const cursor = args?.after ? { id: args.after } : undefined

		const tags = await ctx.prisma.tag.findMany({
			take: take + 1,
			skip,
			cursor,
			where: this.buildWhereCondition(args?.filter),
			orderBy: this.buildOrderBy(args?.sort),
			include: {
				products: {
					include: {
						product: true,
					},
				},
			},
		})

		const hasNextPage = tags.length > take
		const nodes = hasNextPage ? tags.slice(0, -1) : tags

		const edges = nodes.map((tag) => ({
			node: tag,
			cursor: tag.id,
		}))

		return {
			edges,
			pageInfo: {
				hasNextPage,
				hasPreviousPage: false,
				startCursor: edges[0]?.cursor,
				endCursor: edges[edges.length - 1]?.cursor,
			},
			totalCount: await ctx.prisma.tag.count({
				where: this.buildWhereCondition(args?.filter),
			}),
		}
	}

	@Mutation(() => Tag)
	async createTag(@Arg('input') input: TagCreateInput, @Ctx() ctx: Context): Promise<Tag> {
		return ctx.prisma.tag.create({
			data: input,
			include: {
				products: {
					include: {
						product: true,
					},
				},
			},
		})
	}

	@Mutation(() => Tag)
	async updateTag(@Arg('id', () => ID) id: string, @Arg('input') input: TagUpdateInput, @Ctx() ctx: Context): Promise<Tag> {
		return ctx.prisma.tag.update({
			where: { id },
			data: input,
			include: {
				products: {
					include: {
						product: true,
					},
				},
			},
		})
	}

	@Mutation(() => Boolean)
	async deleteTag(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.tag.delete({
			where: { id },
		})
		return true
	}

	@Mutation(() => Boolean)
	async assignTagToProduct(@Arg('tagId', () => ID) tagId: string, @Arg('productId', () => ID) productId: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.productTag.create({
			data: {
				tagId,
				productId,
			},
		})
		return true
	}

	@Mutation(() => Boolean)
	async removeTagFromProduct(@Arg('tagId', () => ID) tagId: string, @Arg('productId', () => ID) productId: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.productTag.delete({
			where: {
				productId_tagId: {
					productId,
					tagId,
				},
			},
		})
		return true
	}

	private buildWhereCondition(filter: any) {
		if (!filter) return undefined

		const where: any = {}

		if (filter.name) {
			where.name = { contains: filter.name.contains || filter.name.equals }
		}

		// Handle AND/OR operations
		if (filter.AND) {
			where.AND = filter.AND.map((f: any) => this.buildWhereCondition(f))
		}

		if (filter.OR) {
			where.OR = filter.OR.map((f: any) => this.buildWhereCondition(f))
		}

		return where
	}

	private buildOrderBy(sort: any) {
		if (!sort) return { name: 'asc' }

		const orderBy: any = {}

		if (sort.name) orderBy.name = sort.name.toLowerCase()

		return orderBy
	}
}
