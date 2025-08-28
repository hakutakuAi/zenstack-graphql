import { Resolver, Query, Mutation, Arg, Ctx, ID, Info, Int, FieldResolver, Root } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Tag, TagCreateInput, TagUpdateInput, TagQueryArgs, TagConnection, TagFilterInput, TagSortInput, ProductTag } from '../../schema'
import { ConnectionBuilder } from '../../schema-helpers'
import type { Context } from './types'

@Resolver(() => Tag)
export class TagResolver {
	@Query(() => Tag, { nullable: true })
	async tag(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<Tag | null> {
		return (await ctx.prisma.tag.findUnique({ where: { id } })) as Tag | null
	}

	@Query(() => TagConnection)
	async tags(
		@Arg('filter', () => TagFilterInput, { nullable: true }) filter: TagFilterInput | undefined,
		@Arg('sort', () => TagSortInput, { nullable: true }) sort: TagSortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() ctx: Context,
	): Promise<TagConnection> {
		const args: TagQueryArgs = { filter, sort, first, after, last, before }
		const config = ConnectionBuilder.buildTagConnectionConfig(args, info)

		const items = await ctx.prisma.tag.findMany(config.findManyOptions)
		const totalCount = await ctx.prisma.tag.count(config.countOptions)

		return ConnectionBuilder.processResults(items, totalCount, config.paginationInfo) as TagConnection
	}

	@Mutation(() => Tag)
	async createTag(@Arg('input', () => TagCreateInput) input: TagCreateInput, @Ctx() ctx: Context): Promise<Tag> {
		return (await ctx.prisma.tag.create({ data: input })) as Tag
	}

	@Mutation(() => Tag)
	async updateTag(@Arg('id', () => ID) id: string, @Arg('input', () => TagUpdateInput) input: TagUpdateInput, @Ctx() ctx: Context): Promise<Tag> {
		return (await ctx.prisma.tag.update({ where: { id }, data: input })) as Tag
	}

	@Mutation(() => Boolean)
	async deleteTag(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.tag.delete({ where: { id } })
		return true
	}

	@Mutation(() => Boolean)
	async assignTagToProduct(@Arg('tagId', () => ID) tagId: string, @Arg('productId', () => ID) productId: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.productTag.create({ data: { tagId, productId } })
		return true
	}

	@Mutation(() => Boolean)
	async removeTagFromProduct(@Arg('tagId', () => ID) tagId: string, @Arg('productId', () => ID) productId: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.productTag.delete({ where: { productId_tagId: { productId, tagId } } })
		return true
	}

	@FieldResolver(() => [ProductTag])
	async products(@Root() tag: Tag, @Ctx() ctx: Context): Promise<ProductTag[]> {
		return (await ctx.prisma.productTag.findMany({ where: { tagId: tag.id } })) as ProductTag[]
	}
}
